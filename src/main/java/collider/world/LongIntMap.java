package collider.world;

import java.util.Arrays;

/// Ints by long key for one pass, never kept or shared. A missing key
/// reads as -1. A key once put stays, whatever its value.
/// In Java because it is a mutable structure on primitive arrays.
public final class LongIntMap {

    private static final int FREE = Integer.MIN_VALUE;

    private long[] keys;
    private int[] vals;
    private int shift;
    private int n;

    /// Returns a map that takes `size` keys without growing.
    public LongIntMap(int size) {
        int c = Integer.highestOneBit(Math.max(8, size) * 2 - 1) * 2;
        keys = new long[c];
        vals = new int[c];
        Arrays.fill(vals, FREE);
        shift = 64 - Integer.numberOfTrailingZeros(c);
    }

    private int slot(long k) {
        return (int) ((k * 0x9E3779B97F4A7C15L) >>> shift);
    }

    public int get(long k) {
        int mask = keys.length - 1;
        for (int i = slot(k); vals[i] != FREE; i = (i + 1) & mask) {
            if (keys[i] == k) return vals[i];
        }
        return -1;
    }

    /// Maps `k` to `v`, which must not be `Integer.MIN_VALUE`, and
    /// returns the value `k` had.
    public int put(long k, int v) {
        int mask = keys.length - 1;
        int i = slot(k);
        for (; vals[i] != FREE; i = (i + 1) & mask) {
            if (keys[i] == k) {
                int old = vals[i];
                vals[i] = v;
                return old;
            }
        }
        keys[i] = k;
        vals[i] = v;
        if (2 * ++n > keys.length) grow();
        return -1;
    }

    private void grow() {
        long[] ks = keys;
        int[] vs = vals;
        keys = new long[2 * ks.length];
        vals = new int[2 * ks.length];
        Arrays.fill(vals, FREE);
        shift--;
        int mask = keys.length - 1;
        for (int j = 0; j < ks.length; j++) {
            if (vs[j] == FREE) continue;
            int i = slot(ks[j]);
            while (vals[i] != FREE) i = (i + 1) & mask;
            keys[i] = ks[j];
            vals[i] = vs[j];
        }
    }
}
