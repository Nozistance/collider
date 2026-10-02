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
        HashMap<Key, Integer> map = new HashMap<>();
        for (int i = 0; i < hashes.length; i++) {
            map.put(new Key(hashes[i], i), i);
        }
        int[] out = new int[hashes.length];
        int k = 0;
        for (int i : map.values()) out[k++] = i;
        return out;
    }
}
