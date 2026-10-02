package collider.world;

import java.util.Arrays;

/// Values by long key for one pass, never kept or shared.
public final class Scratch<V> {

    private long[] keys;
    private Object[] vals;
    private int n;

    public Scratch() {
        this(8);
    }

    /// Returns a map sized for `size` keys.
    public Scratch(int size) {
        int c = Integer.highestOneBit(Math.max(8, size) * 2 - 1) * 2;
        keys = new long[c];
        vals = new Object[c];
    }

    private static int mix(long k, int mask) {
        long h = k * 0x9E3779B97F4A7C15L;
        return (int) (h ^ (h >>> 32)) & mask;
    }

    @SuppressWarnings("unchecked")
    public V get(long k) {
        int mask = keys.length - 1;
        for (int i = mix(k, mask); ; i = (i + 1) & mask) {
            Object v = vals[i];
            if (v == null) return null;
            if (keys[i] == k) return (V) v;
        }
    }

    /// Maps `k` to `v`, which must not be null.
    public void put(long k, V v) {
        if (2 * (n + 1) > keys.length) grow();
        int mask = keys.length - 1;
        for (int i = mix(k, mask); ; i = (i + 1) & mask) {
            if (vals[i] == null) {
                keys[i] = k;
                vals[i] = v;
                n++;
                return;
            }
            if (keys[i] == k) {
                vals[i] = v;
                return;
            }
        }
    }

    @SuppressWarnings("unchecked")
    private void grow() {
        long[] ks = keys;
        Object[] vs = vals;
        keys = new long[2 * ks.length];
        vals = new Object[2 * ks.length];
        n = 0;
        for (int i = 0; i < ks.length; i++) {
            if (vs[i] != null) put(ks[i], (V) vs[i]);
        }
    }

    public boolean isEmpty() {
        return n == 0;
    }

    /// Returns the keys in ascending order.
    public long[] sortedKeys() {
        long[] r = new long[n];
        int j = 0;
        for (int i = 0; i < keys.length; i++) {
            if (vals[i] != null) r[j++] = keys[i];
        }
        Arrays.sort(r);
        return r;
    }
}
