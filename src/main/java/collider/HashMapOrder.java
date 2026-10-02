package collider;

import java.util.HashMap;

/// The order in which a hash map gives back keys of given hash codes.
public final class HashMapOrder {

    private record Key(int hash, int index) {
        @Override
        public int hashCode() {
            return hash;
        }
    }

    /// Returns the indices of the keys with hash codes `hashes`, put in
    /// that order, in the order the map iterates them.
    public static int[] of(int[] hashes) {
        return order(filled(hashes));
    }

    /// Returns the indices of the keys with hash codes `hashes`, put in
    /// that order, in the order a copy of the map iterates them: a new
    /// map given all of them at once, sized by their count.
    public static int[] copied(int[] hashes) {
        return order(new HashMap<>(filled(hashes)));
    }

    private static HashMap<Key, Integer> filled(int[] hashes) {
        HashMap<Key, Integer> map = new HashMap<>();
        for (int i = 0; i < hashes.length; i++) {
            map.put(new Key(hashes[i], i), i);
        }
        return map;
    }

    private static int[] order(HashMap<Key, Integer> map) {
        int[] out = new int[map.size()];
        int k = 0;
        for (int i : map.values()) out[k++] = i;
        return out;
    }
}
