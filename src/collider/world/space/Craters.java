package collider.world.space;

import collider.world.LongMap;

/// The cells that the blasts of a tick changed so far, with the state
/// each holds now. The blasts of a tick take them in order.
public final class Craters {

    private final LongMap<Integer> states = new LongMap<>();
    private final LongMap<long[]> buckets = new LongMap<>();
    private int n;

    private static long key(int x, int y, int z) {
        return ((long) (x & 0x3FFFFFF) << 38)
            | ((long) (z & 0x3FFFFFF) << 12) | (y & 0xFFF);
    }

    /// Notes in `c` that the cell at `x`, `y`, `z` now holds `st`.
    public static void note(Craters c, int x, int y, int z, int st) {
        c.put(x, y, z, st);
    }

    /// Returns the state the cell at `x`, `y`, `z` holds now in `c`,
    /// or -1 when no blast changed it.
    public static int now(Craters c, int x, int y, int z) {
        return c.state(x, y, z);
    }

    /// Returns true when no cell changed.
    public boolean isEmpty() {
        return n == 0;
    }

    /// Notes that the cell at `x`, `y`, `z` now holds `st`.
    public void put(int x, int y, int z, int st) {
        long k = key(x, y, z);
        if (states.get(k) == null) {
            long b = key(x >> 2, y >> 2, z >> 2);
            long[] m = buckets.get(b);
            if (m == null) {
                m = new long[1];
                buckets.put(b, m);
            }
            m[0] |= 1L << bit(x, y, z);
            n++;
        }
        states.put(k, st);
    }

    private static int bit(int x, int y, int z) {
        return ((x & 3) << 4) | ((y & 3) << 2) | (z & 3);
    }

    /// Returns the state the cell at `x`, `y`, `z` holds now, or -1
    /// when no blast changed it.
    public int state(int x, int y, int z) {
        if (n == 0) return -1;
        Integer s = states.get(key(x, y, z));
        return s == null ? -1 : s;
    }

    /// Returns true when a changed cell lies in the box of cells from
    /// `x0`, `y0`, `z0` to `x1`, `y1`, `z1`, both included.
    public boolean anyIn(int x0, int y0, int z0, int x1, int y1,
            int z1) {
        if (n == 0) return false;
        for (int bx = x0 >> 2; bx <= x1 >> 2; bx++) {
            for (int by = y0 >> 2; by <= y1 >> 2; by++) {
                for (int bz = z0 >> 2; bz <= z1 >> 2; bz++) {
                    long[] m = buckets.get(key(bx, by, bz));
                    if (m == null) continue;
                    for (long r = m[0]; r != 0; r &= r - 1) {
                        int i = Long.numberOfTrailingZeros(r);
                        int x = (bx << 2) | (i >> 4);
                        int y = (by << 2) | ((i >> 2) & 3);
                        int z = (bz << 2) | (i & 3);
                        if (x >= x0 && x <= x1 && y >= y0 && y <= y1
                                && z >= z0 && z <= z1) return true;
                    }
                }
            }
        }
        return false;
    }

    /// Returns the changed cells in the cube of width `w` at `ox`,
    /// `oy`, `oz` as pairs of their index in the cube, x major, and
    /// their state.
    public int[] inCube(int ox, int oy, int oz, int w) {
        if (n == 0) return new int[0];
        int[] out = new int[16];
        int m = 0;
        int x1 = ox + w - 1, y1 = oy + w - 1, z1 = oz + w - 1;
        for (int bx = ox >> 2; bx <= x1 >> 2; bx++) {
            for (int by = oy >> 2; by <= y1 >> 2; by++) {
                for (int bz = oz >> 2; bz <= z1 >> 2; bz++) {
                    long[] a = buckets.get(key(bx, by, bz));
                    if (a == null) continue;
                    for (long r = a[0]; r != 0; r &= r - 1) {
                        int i = Long.numberOfTrailingZeros(r);
                        int x = (bx << 2) | (i >> 4);
                        int y = (by << 2) | ((i >> 2) & 3);
                        int z = (bz << 2) | (i & 3);
                        int ix = x - ox, iy = y - oy, iz = z - oz;
                        if (ix < 0 || ix >= w || iy < 0 || iy >= w
                                || iz < 0 || iz >= w) continue;
                        if (m + 2 > out.length) {
                            out = java.util.Arrays.copyOf(out, 2 * out.length);
                        }
                        out[m] = (ix * w + iy) * w + iz;
                        out[m + 1] = states.get(key(x, y, z));
                        m += 2;
                    }
                }
            }
        }
        return java.util.Arrays.copyOf(out, m);
    }
}
