(ns collider.tables.load
  "The data of the vanilla pack, loaded the way a dedicated server
  loads it on its first start."
  (:require [collider.tables.reflect
             :refer [*loader* call call-static cls static-field]])
  (:import (clojure.lang Reflector)
           (java.lang.reflect InvocationHandler Method Proxy)
           (java.util.concurrent Executor Future)))

(set! *warn-on-reflection* true)

(defn- construct [c & args]
  (Reflector/invokeConstructor (cls c) (object-array args)))

(defn- implement
  "Returns an instance of the functional interface c of the server
  whose single method m calls f with its arguments, on any thread."
  [c ^String m f]
  (let [f (bound-fn* f)
        handler (reify InvocationHandler
                  (invoke [_ _ method args]
                    (if (= m (Method/.getName method))
                      (apply f args)
                      (throw (UnsupportedOperationException. m)))))]
    (Proxy/newProxyInstance
      *loader* (into-array Class [(cls c)]) handler)))

(defn- normal-dimensions [worldgen]
  (call-static "world.level.levelgen.presets.WorldPresets"
               "createNormalWorldDimensions" worldgen))

(defn- dimensions [ctx]
  (let [stems (static-field "core.registries.Registries" "LEVEL_STEM")
        base (-> (call ctx "datapackDimensions")
                 (call "lookupOrThrow" stems))
        dims (normal-dimensions (call ctx "datapackWorldgen"))
        access (-> (call dims "bake" base)
                   (call "dimensionsRegistryAccess"))]
    (construct "server.WorldLoader$DataLoadOutput" nil access)))

(defn- data-config []
  (static-field "world.level.WorldDataConfiguration" "DEFAULT"))

(defn- pack-config []
  (let [source "server.packs.repository.ServerPacksSource"
        repo (call-static source "createVanillaTrustedRepository")]
    (construct "server.WorldLoader$PackConfig"
               repo (data-config) false true)))

(defn- init-config []
  (let [packs (pack-config)
        selection "commands.Commands$CommandSelection"
        permissions "server.permissions.LevelBasedPermissionSet"]
    (construct "server.WorldLoader$InitConfig" packs
               (static-field selection "DEDICATED")
               (static-field permissions "GAMEMASTER"))))

(def ^:private now
  (reify Executor (execute [_ r] (Runnable/.run r))))

(defn- loaded [resources managers layers _]
  {:resources resources :managers managers
   :access (call layers "compositeAccess")})

(defn- finish-recipes! [managers]
  (call (call managers "getRecipeManager") "finalizeRecipeLoading"
        (call (data-config) "enabledFeatures")))

(defn- supplier []
  (implement "server.WorldLoader$WorldDataSupplier" "get"
             dimensions))

(defn- result []
  (implement "server.WorldLoader$ResultFactory" "create" loaded))

(defn- loaded-world []
  (let [pool (call-static "util.Util" "backgroundExecutor")
        f (call-static "server.WorldLoader" "load" (init-config)
                       (supplier) (result) pool now)]
    (Future/.get f)))

(defn load-world
  "Loads the vanilla pack as a dedicated server with no level on disk
  does, and finishes the recipes as the server does on start. Returns
  the resource manager, the reloadable resources and the registries.
  The caller closes the resource manager."
  []
  (let [world (loaded-world)]
    (finish-recipes! (:managers world))
    world))
