# collider-tables

Makes the game data tables that Collider reads, from the official
vanilla server jar. Needs Java 25.

```sh
# download the server jar from Mojang, write the tables into data/
java -jar collider-tables.jar generate

# use a server jar you already have
java -jar collider-tables.jar generate --jar server.jar

# write the tables next to the server
java -jar collider-tables.jar generate --out /data

# tell if data/ holds a full set of tables
java -jar collider-tables.jar check data
```

## Build from source

Needs Java 25 and the [Clojure CLI](https://clojure.org/guides/install_clojure).

```sh
# gives target/collider-tables.jar
clojure -T:build uber

# from the root of Collider: run from source, tables into target/data
clojure -T:build tables
```
