(ns collider.tables.fuel
  "The items a furnace burns and for how long.")

(set! *warn-on-reflection* true)

(def ^:private fuel-values
  [[:lava-bucket 20000] [:coal-block 16000] [:blaze-rod 2400]
   [:coal 1600] [:charcoal 1600]
   [{:tag "logs"} 300] [{:tag "bamboo_blocks"} 300]
   [{:tag "planks"} 300] [:bamboo-mosaic 300]
   [{:tag "wooden_stairs"} 300] [:bamboo-mosaic-stairs 300]
   [{:tag "wooden_slabs"} 150] [:bamboo-mosaic-slab 150]
   [{:tag "wooden_trapdoors"} 300]
   [{:tag "wooden_pressure_plates"} 300]
   [{:tag "wooden_shelves"} 300] [{:tag "wooden_fences"} 300]
   [{:tag "fence_gates"} 300] [:note-block 300] [:bookshelf 300]
   [:chiseled-bookshelf 300] [:lectern 300] [:jukebox 300]
   [:chest 300] [:trapped-chest 300] [:crafting-table 300]
   [:daylight-detector 300] [{:tag "banners"} 300] [:bow 300]
   [:fishing-rod 300] [:ladder 300]
   [{:tag "signs"} 200] [{:tag "hanging_signs"} 800]
   [:wooden-shovel 200] [:wooden-sword 200] [:wooden-spear 200]
   [:wooden-hoe 200] [:wooden-axe 200] [:wooden-pickaxe 200]
   [{:tag "wooden_doors"} 200] [{:tag "boats"} 1200]
   [{:tag "wool"} 100] [{:tag "wooden_buttons"} 100] [:stick 100]
   [{:tag "saplings"} 100] [:bowl 100]
   [{:tag "wool_carpets"} 67] [:dried-kelp-block 4001]
   [:crossbow 300] [:bamboo 50] [:dead-bush 100]
   [:short-dry-grass 100] [:tall-dry-grass 100] [:scaffolding 50]
   [:loom 300] [:barrel 300] [:cartography-table 300]
   [:fletching-table 300] [:smithing-table 300] [:composter 300]
   [:azalea 100] [:flowering-azalea 100] [:mangrove-roots 300]
   [:leaf-litter 100]])

(defn- fuel-targets [tags target]
  (if (map? target) (get-in tags ["item" (:tag target)] []) [target]))

(defn fuel
  "Returns how many ticks each item burns for in a furnace."
  [tags items]
  (let [add (fn [m [target time]]
              (into m (comp (filter items) (map #(vector % time)))
                    (fuel-targets tags target)))]
    (apply dissoc (reduce add (sorted-map) fuel-values)
           (get-in tags ["item" "non_flammable_wood"] []))))
