# Collider

### A proof-of-concept Minecraft server

## About

Collider is a Minecraft server with a parallel tick and no asterisks: no locks,
no regions, no thread ownership, no rules about which mechanics may cross a
boundary. Every part of it is as simple as I could make it.

On one core it matches vanilla 1.8.9 performance; on six it is 4.5 times faster.

## FAQ

**Why Clojure?**

Servers that try to parallelize the tick end up reaching for the same ideas: an
immutable snapshot, a pure read phase, changes as data instead of in-place
edits. Each gets one or two of them, hand-written, for one subsystem. I did not
have to collect them: the language hands over all of it at once. The whole tick,
six lines:

```clojure
(defn tick [world events]
  (let [world' (-> (update world :tick inc)
                   (update :time-of-day (fnil inc 0)))
        world' (reduce state/apply-event world' events)
        deltas (deltas/of systems world' events)]
    (state/apply-deltas world' deltas)))
```

**Which Minecraft projects is this for?**

None. The feature set is deliberately small - the point was to be able to
measure performance, and to have a reference for working with the data model.
It is a personal experiment, ~5500 lines of Clojure.

**How does this compare to the region (Folia) approach?**

Regions need spatial independence: split the world, give each part a thread,
never let two touch. A crowd merges neighboring regions back into one thread.
Collider does not care where the players are.

## What is implemented

The only mob is the sheep, with a closely reproduced AI. There is light,
water, lava, fire, block physics, dropped items, TNT, damage, a day and night
cycle, world saving, chat with commands, and an inventory.

World: Minecraft 1.8.9, flat, creative, peaceful.

Some features exist to load the benchmarks, the rest to prove the data model
carries real mechanics.

## How to run it

```bash
java \
  -XX:+UseG1GC -XX:MaxGCPauseMillis=5 \
  -XX:G1PeriodicGCInterval=60000 -Xmx2g \
  --sun-misc-unsafe-memory-access=allow \
  -jar collider.jar
```

Requires Java 21+. Server properties go in a `config.edn` file in the working
directory.

## Credits

To recreate vanilla behavior I read the decompiled 1.8.9 sources of
[MCP-919](https://github.com/Marcelektro/MCP-919).
