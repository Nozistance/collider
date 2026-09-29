package collider.game.mob;

import clojure.lang.ITransientCollection;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/// The bodies of one island by the block column they stand in, for
/// the shoves between overlapping bodies. It changes in place as the
/// bodies move, so one island steps with one grid. Hits come in the
/// order of the push cells of four by four columns around the body,
/// then by id.
public final class PushGrid extends AbstractMap<Long, PushCell> {

    private static final double STRENGTH = (double) 0.05F;

    private static final double THRESHOLD = (double) 0.01F;

    private final long[] eids;
    private final double[] halfs, heights, xs, ys, zs;
    private final int[] next;
    private long[] keys;
    private int[] heads;
    private int used;
    private double widest;
    private int[] hits = new int[16];

    /// Returns the grid of bodies `eids`, ascending, each with half
    /// width, height and position at the same index.
    public PushGrid(long[] eids, double[] halfs, double[] heights,
                    double[] xs, double[] ys, double[] zs) {
        this.eids = eids;
        this.halfs = halfs;
        this.heights = heights;
        this.xs = xs;
        this.ys = ys;
        this.zs = zs;
        int n = eids.length;
        this.next = new int[n];
        int cap = 16;
        while (cap < 4 * n) cap <<= 1;
        this.keys = new long[cap];
        this.heads = new int[cap];
        Arrays.fill(heads, -1);
        for (int i = 0; i < n; i++) {
            widest = Math.max(widest, halfs[i]);
            link(i);
        }
    }

    private static long key(long cx, long cz) {
        return ((cx & 0xFFFFFFFFL) << 32) | (cz & 0xFFFFFFFFL);
    }

    private static long column(double x, double z) {
        return key((long) Math.floor(x), (long) Math.floor(z));
    }

    private int head(long k) {
        int m = keys.length - 1;
        int i = slot0(k, m);
        while (heads[i] != -1) {
            if (keys[i] == k) return Math.max(heads[i], -1);
            i = (i + 1) & m;
        }
        return -1;
    }

    private static int slot0(long k, int m) {
        long h = k * 0x9E3779B97F4A7C15L;
        return (int) (h ^ (h >>> 32)) & m;
    }

    private void link(int s) {
        long k = column(xs[s], zs[s]);
        int m = keys.length - 1;
        int i = slot0(k, m);
        while (heads[i] != -1) {
            if (keys[i] == k) {
                next[s] = Math.max(heads[i], -1);
                heads[i] = s;
                return;
            }
            i = (i + 1) & m;
        }
        keys[i] = k;
        next[s] = -1;
        heads[i] = s;
        if (++used * 2 > keys.length) grow();
    }

    private void unlink(int s) {
        long k = column(xs[s], zs[s]);
        int m = keys.length - 1;
        int i = slot0(k, m);
        while (heads[i] == -1 || keys[i] != k) i = (i + 1) & m;
        if (heads[i] == s) {
            heads[i] = next[s] < 0 ? -2 : next[s];
            return;
        }
        int p = heads[i];
        while (next[p] != s) p = next[p];
        next[p] = next[s];
    }

    private void grow() {
        long[] ok = keys;
        int[] oh = heads;
        keys = new long[ok.length * 2];
        heads = new int[ok.length * 2];
        Arrays.fill(heads, -1);
        int m = keys.length - 1;
        used = 0;
        for (int j = 0; j < ok.length; j++) {
            if (oh[j] >= 0) {
                int i = slot0(ok[j], m);
                while (heads[i] != -1) i = (i + 1) & m;
                keys[i] = ok[j];
                heads[i] = oh[j];
                used++;
            }
        }
    }

    /// Moves the body `eid` of grid `g` to `x`, `y`, `z` with half width `half`
    /// and height `height`, and returns this grid.
    public static PushGrid moved(PushGrid g, long eid, double half,
                                 double height, double x, double y,
                                 double z) {
        return g.move(eid, half, height, x, y, z);
    }

    private PushGrid move(long eid, double half, double height,
                          double x, double y, double z) {
        int s = Arrays.binarySearch(eids, eid);
        boolean same = column(x, z) == column(xs[s], zs[s]);
        if (!same) unlink(s);
        halfs[s] = half;
        heights[s] = height;
        xs[s] = x;
        ys[s] = y;
        zs[s] = z;
        widest = Math.max(widest, half);
        if (!same) link(s);
        return this;
    }

    private int overlapping(double x, double y, double z, double half,
                            double height, long eid, long hi) {
        double r = half + widest;
        long x0 = (long) Math.floor(x - r), x1 = (long) Math.floor(x + r);
        long z0 = (long) Math.floor(z - r), z1 = (long) Math.floor(z + r);
        long cx = Math.floorDiv((long) Math.floor(x), 4);
        long cz = Math.floorDiv((long) Math.floor(z), 4);
        int n = 0;
        for (long bx = x0; bx <= x1; bx++) {
            long dx = Math.floorDiv(bx, 4) - cx;
            if (dx < -1 || dx > 1) continue;
            for (long bz = z0; bz <= z1; bz++) {
                long dz = Math.floorDiv(bz, 4) - cz;
                if (dz < -1 || dz > 1) continue;
                int order = (int) ((dx + 1) * 3 + dz + 1) * eids.length;
                for (int j = head(key(bx, bz)); j >= 0; j = next[j]) {
                    long o = eids[j];
                    if (o == eid || o >= hi) continue;
                    double rj = half + halfs[j];
                    double oy = ys[j];
                    if (Math.abs(xs[j] - x) < rj
                            && Math.abs(zs[j] - z) < rj
                            && oy < y + height && oy + heights[j] > y) {
                        if (n == hits.length) {
                            hits = Arrays.copyOf(hits, n * 2);
                        }
                        hits[n++] = order + j;
                    }
                }
            }
        }
        Arrays.sort(hits, 0, n);
        return n;
    }

    /// Returns the shoves between the body `eid` of half width `half`
    /// and height `height` at `x`, `y`, `z` and each body it overlaps
    /// whose id is below `hi`. Each shove `[id dx dz]` moves this body
    /// by dx dz and the other body the opposite way. The shoves are
    /// not summed.
    public static Object shoves(PushGrid g, double x, double y,
                                double z, double half, double height,
                                long eid, long hi) {
        return g.shoved(x, y, z, half, height, eid, hi);
    }

    private Object shoved(double x, double y, double z, double half,
                          double height, long eid, long hi) {
        int n = overlapping(x, y, z, half, height, eid, hi);
        ITransientCollection acc = PersistentVector.EMPTY.asTransient();
        int k = eids.length;
        for (int i = 0; i < n; i++) {
            int j = hits[i] % k;
            double dx = x - xs[j], dz = z - zs[j];
            double m = Math.max(Math.abs(dx), Math.abs(dz));
            if (m >= THRESHOLD) {
                double s = Math.sqrt(m);
                double p = Math.min(1.0, 1.0 / s);
                acc.conj(RT.vector(eids[j], dx / s * p * STRENGTH,
                                   dz / s * p * STRENGTH));
            }
        }
        return acc.persistent();
    }

    /// Returns the ids of the bodies whose boxes overlap the box of
    /// the body `eid` of half width `half` and height `height` at
    /// `x`, `y`, `z`.
    public static Object touching(PushGrid g, double x, double y,
                                  double z, double half, double height,
                                  long eid) {
        return g.touched(x, y, z, half, height, eid);
    }

    private Object touched(double x, double y, double z, double half,
                           double height, long eid) {
        int n = overlapping(x, y, z, half, height, eid, Long.MAX_VALUE);
        ITransientCollection acc = PersistentVector.EMPTY.asTransient();
        int k = eids.length;
        for (int i = 0; i < n; i++) acc.conj(eids[hits[i] % k]);
        return acc.persistent();
    }

    /// Returns the bodies of each push cell of four by four columns,
    /// by the key of the cell.
    @Override
    public Set<Map.Entry<Long, PushCell>> entrySet() {
        Map<Long, int[]> in = new HashMap<>();
        for (int i = 0; i < eids.length; i++) {
            long k = key(Math.floorDiv((long) Math.floor(xs[i]), 4),
                         Math.floorDiv((long) Math.floor(zs[i]), 4));
            int[] c = in.getOrDefault(k, new int[0]);
            c = Arrays.copyOf(c, c.length + 1);
            c[c.length - 1] = i;
            in.put(k, c);
        }
        Map<Long, PushCell> out = new HashMap<>();
        in.forEach((k, c) -> out.put(k, cell(c)));
        return out.entrySet();
    }

    private PushCell cell(int[] c) {
        int n = c.length;
        long[] e = new long[n];
        double[] h = new double[n], t = new double[n], x = new double[n],
                y = new double[n], z = new double[n];
        for (int i = 0; i < n; i++) {
            int j = c[i];
            e[i] = eids[j];
            h[i] = halfs[j];
            t[i] = heights[j];
            x[i] = xs[j];
            y[i] = ys[j];
            z[i] = zs[j];
        }
        return new PushCell(e, h, t, x, y, z);
    }
}
