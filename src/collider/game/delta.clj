(ns collider.game.delta
  "Словарь дельт: всё, что система может выдать за тик, в одном месте.

   Дельта — вектор `[tag & args]`. `deltas/add` раскладывает их в три корзины:
   мировые (меняют мир), сущностные (по eid, теги из `deltas/entity-tags`)
   и эффекты `[:fx msg]` — сообщения игрокам, карты с `:msg` (строятся в
   `game/out.clj`, рендерятся в `render.clj`). Применяет всё `game/state.clj`.

   Схемы — malli. Проверка не в горячем пути: `check!` зовётся из тика,
   только когда `validate?` истинно (тесты, живой REPL)."
  (:require [malli.core :as m]
            [malli.error :as me]))

;; --- примитивы ------------------------------------------------------------

(def Pos "Позиция блока [x y z]." [:tuple :int :int :int])
(defn- vec3? [v]
  (or (instance? collider.java.V3 v)
      (and (sequential? v) (= 3 (count v)) (every? number? v))))
(def Vec3 "Вектор в блоках: [x y z] из double или примитивный V3 горячего пути." [:fn vec3?])
(def Eid "Id сущности; игроки с 1, остальные с 1 000 000." :int)
(def State "Глобальный id состояния блока 26.2." :int)
(def Stack "Стек предмета." [:map [:item :keyword] [:count :int]])
(def Records "Пачка блоков [[pos state] …]." [:sequential [:tuple Pos State]])
(def Coll "Любая коллекция (int-set, вектор, список)." [:fn coll?])
(def Runs "Текст чата: строки или карты translate/with." [:sequential [:or :string :map]])

;; --- мировые дельты -------------------------------------------------------

(def world-deltas
  "tag → [схема аргументов, описание]."
  {:set-blocks
   [[:cat Records]
    "Записать блоки с обновлением соседей. Клиенты узнают в конце того же
     тика одной пачкой на чанк (ChunkHolder.broadcastChanges): tick.clj
     сбрасывает :block-events в эффекты :blocks-changed."]
   :ticks-flushed
   [[:cat :int Coll]
    "Блок-тики до t включительно исполнены: снять; parked (были в неактивных
     чанках) остаются просроченными и идут, как только чанк активен."]
   :schedule-ticks
   [[:cat [:map-of :int Coll]]
    "Правило просит тикнуть клетки снова без смены блока (scheduleTick из tick
     в ванили, огонь): {тик [block-id …]}."]
   :block-events-flushed
   [[:cat]
    "Очередь block events отправлена, очистить."]
   :set-time
   [[:cat :int]
    "Время суток (команда /time, сон)."]
   :set-rule
   [[:cat :keyword :any]
    "Геймрул: ключ из game/rules и значение."]
   :spawn-entity
   [[:cat :map]
    "Новая сущность из карты (entity/of); eid выдаёт мир."]
   :remove-entity
   [[:cat Eid]
    "Убрать сущность; для игрока — уход с сервера."]
   :listed
   [[:cat [:map-of Eid :uuid] Coll]
    "Кто в таб-листе (:listed мира): добавить {eid uuid}, убрать eids.
     Нужно, чтобы снять из списка игрока, чьей сущности уже нет."]})

;; --- дельты сущностей -----------------------------------------------------

(def entity-deltas
  "tag → [схема аргументов после eid, описание]. Первый аргумент всегда eid."
  {:merge-entity
   [[:cat :map]
    "Слить поля в сущность (так же снимается :needs-spawn? после спавна)."]
   :teleport
   [[:cat Vec3]
    "Поставить игрока в точку и ждать teleport-ack: :pos, :tp-target,
     :tp-id = тик (сон, респавн, /tp, повтор через 20 тиков)."]
   :client-slots
   [[:cat [:map-of :int [:maybe Stack]] [:maybe Stack]]
    "Что клиент сам поставил в слоты и на курсор (клик, creative-slot):
     копия remoteSlots в Track, render шлёт только расхождения."]
   :track
   [[:cat :map]
    "Что клиенты уже знают о сущности (Track: pos, yaw, mdata, equip,
     vel-sent; для игрока ещё slots и carried — его remoteSlots; :seen).
     Система игроков шлёт разницу и обновляет."]
   :tracking
   [[:cat Coll Coll]
    "Кого этот игрок видит: добавить eids, убрать eids. Спавн и снятие
     сущностей клиенту render выводит отсюда."]
   :set-slot
   [[:cat :int [:maybe Stack]]
    "Слот инвентаря: стек или nil (пусто)."]
   :chunks-sent
   [[:cat Coll Coll]
    "Чанки игроку: добавленные и убранные ids (render шлёт чанки сам,
     центр берёт из :chunk-pos сущности)."]
   :damage
   [[:cat number? [:? [:cat number? number?]]]
    "Урон: величина и, если есть, направление отброса dx dz."]
   :push
   [[:cat Vec3]
    "Прибавить скорость (у TNT — отдачу :kb)."]})

;; --- эффекты --------------------------------------------------------------

(def fx-messages
  "msg → [поля, описание]. Адрес добавляется поверх: `:to eid` личное,
   `:except eid` всем кроме, без адреса — всем, кого касается (решает render)."
  {:blocks-changed [[[:cp :int] [:records Records]] "Блоки чанка, сменившиеся за тик; одна запись — block-update, больше — section update. С :to — правка одному игроку (отказ в установке)."]
   :break-effect   [[[:pos Pos] [:state State]] "Частицы и звук разрушения блока."]
   :explosion      [[[:center Vec3] [:radius number?] [:blocks :int] [:motion Vec3]] "Взрыв: центр, радиус, сколько блоков снесено (клиент рисует дым по числу), толчок адресату."]
   :sound          [[[:kind :keyword] [:pos Vec3] [:volume number?] [:pitch number?]] "Звук в точке."]
   :particles      [[[:kind :keyword] [:state [:maybe State]] [:pos Vec3] [:count :int] [:speed number?]] "Частицы."]
   :extinguish     [[[:pos Pos]] "Огонь потушен (level event 1009)."]
   :fizz           [[[:pos Pos]] "Шипение: лава с водой, огонь в воде."]
   :time           [[[:age :int] [:time :int]] "Часы: возраст мира и время суток после тика."]
   :teleport       [[[:pos Vec3] [:yaw number?] [:pitch number?]] "Поставить игрока сюда (ждём teleport-ack)."]
   :health         [[[:health number?]] "Здоровье игрока."]
   :respawn        [[] "Возродить игрока."]
   :keepalive      [[[:id :int]] "Пинг."]
   :disconnect     [[[:text [:or :string :map]]] "Отключить с текстом (строка или translate)."]
   :close          [[] "Закрыть соединение."]
   :block-ack      [[[:sequence :int]] "Подтвердить клиенту его правки блоков до sequence."]
   :set-slot       [[[:slot :int] [:stack [:maybe Stack]]] "Слот инвентаря клиенту."]
   :carried        [[[:stack [:maybe Stack]]] "Стек на курсоре."]
   :held-slot      [[[:slot :int]] "Выбранный слот хотбара."]
   :inventory      [[[:slots [:sequential :any]] [:carried [:maybe Stack]]] "Весь инвентарь."]
   :suggestions    [[[:id :int] [:start :int] [:length :int] [:matches [:sequential :any]]] "Подсказки команд."]
   :system-chat    [[[:runs Runs]] "Системное сообщение в чат."]
   :player-chat    [[[:name :string] [:runs Runs]] "Сообщение игрока."]
   :overlay        [[[:runs Runs]] "Текст над хотбаром."]
   :stats          [[[:stats :map]] "Статистика для экрана клиента."]
   :game-rules     [[[:rules :map]] "Геймрулы для экрана клиента."]
   :tab-add        [[[:entries [:sequential :map]]] "В таб-лист."]
   :tab-remove     [[[:uuids [:sequential :uuid]]] "Из таб-листа."]
   :tab-latency    [[[:entries [:sequential :map]]] "Пинги в таб-листе."]
   :tab-header     [[[:header :string] [:footer :string]] "Шапка и подвал таб-листа."]
   :move           [[[:eid Eid] [:dx :int] [:dy :int] [:dz :int] [:on-ground :boolean]] "Сущность сдвинулась (1/4096 блока)."]
   :move-look      [[[:eid Eid] [:dx :int] [:dy :int] [:dz :int] [:yaw :int] [:pitch :int] [:on-ground :boolean]] "Сдвинулась и повернулась."]
   :look           [[[:eid Eid] [:yaw :int] [:pitch :int] [:on-ground :boolean]] "Повернулась."]
   :sync-pos       [[[:eid Eid] [:pos Vec3] [:yaw number?] [:pitch number?] [:on-ground :boolean]] "Абсолютная позиция (накопилась ошибка или далеко)."]
   :head-look      [[[:eid Eid] [:yaw number?]] "Поворот головы."]
   :meta           [[[:eid Eid] [:meta :map]] "Метаданные: горит, крадётся, бежит, скин."]
   :velocity       [[[:eid Eid] [:vel Vec3]] "Скорость сущности."]
   :equipment      [[[:eid Eid] [:slot :int] [:stack [:maybe Stack]]] "Экипировка: слот 0..5 (рука, вторая рука, ботинки … шлем)."]
   :animation      [[[:eid Eid] [:kind :keyword]] "Анимация: удар, пробуждение."]
   :status         [[[:eid Eid] [:kind :keyword]] "Entity event: урон, смерть, стрижка."]
   :collect        [[[:item Eid] [:collector Eid]] "Предмет подобран."]})

;; --- сборка схем ----------------------------------------------------------

(defn- with-address [fields]
  (into [:map [:msg :keyword] [:to {:optional true} Eid] [:except {:optional true} Eid]] fields))

(def Fx
  (into [:multi {:dispatch :msg}]
        (for [[msg [fields _]] fx-messages] [msg (with-address fields)])))

(def Delta
  (into [:multi {:dispatch first}]
        (concat (for [[tag [args _]] world-deltas] [tag (into [:cat [:= tag]] (rest args))])
                (for [[tag [args _]] entity-deltas] [tag (into [:cat [:= tag] Eid] (rest args))])
                [[:fx [:cat [:= :fx] Fx]]])))

(def ^:private delta-validator (m/validator Delta))
(def ^:private delta-explainer (m/explainer Delta))

(defn valid? [delta] (delta-validator delta))

(defn explain
  "Человеческое объяснение, чем дельта плоха, или nil."
  [delta]
  (some-> (delta-explainer delta) me/humanize))

(def validate?
  "Проверять ли дельты каждого тика. Ставится alter-var-root из тестов и
   dev/live.clj; в проде false — горячий путь чист."
  false)

(defn check!
  "Бросает ex-info на первой невалидной дельте."
  [deltas]
  (doseq [d deltas]
    (when-not (delta-validator d)
      (throw (ex-info (str "плохая дельта " (first d)) {:delta d :why (explain d)}))))
  deltas)

(defn describe
  "Описание вида дельты или эффекта для человека."
  [tag]
  (or (second (world-deltas tag)) (second (entity-deltas tag))
      (second (fx-messages tag))))
