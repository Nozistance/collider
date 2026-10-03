package collider.game.mob;

import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/// The bodies of one island by the block column they stand in, for
/// the shoves between overlapping bodies. It changes in place as the
/// bodies move, so one island steps with one grid. Threads may read
/// it at once while no body moves. A body meets the others in the
/// order of the pushable entities of the level. Bodies go by entity
/// section in the order of the section storage, and in each section
/// in the order they came into it. Shoves from the bodies that stepped
/// before come in the order they stepped, by id.
public final class PushGrid extends AbstractMap<Long, PushCell> {

    private static final double STRENGTH = 0.05F;

    private static final double THRESHOLD = 0.01F;

    private final long[] eids;
    private final double[] halfs, heights, xs, ys, zs;
    private final long[] came, ranks;
    private final int[] next;
    private long[] keys;
    private int[] heads;
    private int used;
    private double widest, slack;
    private double[] px, pz, ph;

    /// Returns the grid of bodies `eids`, ascending, each with half
    /// width, height, position, and the tick and rank in it at which
    /// it came into its section at the same index.
    public PushGrid(
            long[] eids,
            double[] halfs,
            double[] heights,
            double[] xs,
            double[] ys,
            double[] zs,
            long[] came,
            long[] ranks) {
        this.eids = eids;
        this.came = came;
        this.ranks = ranks;
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
        return Long.hashCode(h) & m;
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
    /// and height `height`, come into its section at tick `c` and rank
    /// `r`, and returns this grid.
    public static PushGrid moved(
            PushGrid g,
            long eid,
            double half,
            double height,
            double x,
            double y,
            double z,
            long c,
            long r
    ) {
        return g.move(eid, half, height, x, y, z, c, r);
    }

    private PushGrid move(
            long eid,
            double half,
            double height,
            double x,
            double y,
            double z,
            long c,
            long r
    ) {
        int s = Arrays.binarySearch(eids, eid);
        came[s] = c;
        ranks[s] = r;
        boolean same = slack > 0.0 || column(x, z) == column(xs[s], zs[s]);
        if (!same) unlink(s);
        halfs[s] = half;
        heights[s] = height;
        xs[s] = x;
        ys[s] = y;
        zs[s] = z;
        if (slack == 0.0) widest = Math.max(widest, half);
        if (!same) link(s);
        return this;
    }

    /// Keeps each body of grid `g` in the column it stands in now,
    /// for the moves of a tick in which no body moves further than
    /// `reach` along x or z, and no body changes its box. Threads may
    /// then read it while bodies move, and returns `g`.
    public static PushGrid pinned(PushGrid g, double reach) {
        g.slack = reach;
        g.px = g.xs.clone();
        g.pz = g.zs.clone();
        g.ph = g.halfs.clone();
        return g;
    }

    private static long cell(double c) {
        return Math.floorDiv((long) Math.floor(c), 4);
    }

    private int[] overlapping(
            double x,
            double y,
            double z,
            double half,
            double height,
            long eid,
            long hi
    ) {
        double r = half + widest, w = r + slack;
        long x0 = (long) Math.floor(x - w), x1 = (long) Math.floor(x + w);
        long z0 = (long) Math.floor(z - w), z1 = (long) Math.floor(z + w);
        long cx = cell(x), cz = cell(z);
        boolean pinned = slack > 0.0;
        int[] hits = new int[16];
        int n = 0;
        for (long bx = x0; bx <= x1; bx++) {
            long dx = Math.floorDiv(bx, 4) - cx;
            if (!pinned && (dx < -1 || dx > 1)) continue;
            for (long bz = z0; bz <= z1; bz++) {
                long dz = Math.floorDiv(bz, 4) - cz;
                if (!pinned && (dz < -1 || dz > 1)) continue;
                for (int j = head(key(bx, bz)); j >= 0; j = next[j]) {
                    long o = eids[j];
                    if (o == eid || o >= hi) continue;
                    double rj = half + halfs[j];
                    double oy = ys[j];
                    if (Math.abs(xs[j] - x) < rj
                            && Math.abs(zs[j] - z) < rj
                            && oy < y + height
                            && oy + heights[j] > y) {
                        long ox = cell(xs[j]) - cx, oz = cell(zs[j]) - cz;
                        if (ox < -1 || ox > 1 || oz < -1 || oz > 1) continue;
                        if (n == hits.length) {
                            hits = Arrays.copyOf(hits, n * 2);
                        }
                        hits[n++] = j;
                    }
                }
            }
        }
        if (hi == Long.MAX_VALUE) {
            listed(hits, n);
        } else {
            Arrays.sort(hits, 0, n);
        }
        return Arrays.copyOf(hits, n);
    }

    private static long section(double c) {
        return Math.floorDiv((long) Math.floor(c), 16);
    }

    private int compareListed(int a, int b) {
        int c = Long.compare(section(xs[a]), section(xs[b]));
        if (c != 0) return c;
        c = Long.compare(section(zs[a]) & 0x3FFFFFL, section(zs[b]) & 0x3FFFFFL);
        if (c != 0) return c;
        c = Long.compare(section(ys[a]) & 0xFFFFFL, section(ys[b]) & 0xFFFFFL);
        if (c != 0) return c;
        c = Long.compare(came[a], came[b]);
        return c != 0 ? c : Long.compare(ranks[a], ranks[b]);
    }

    private void listed(int[] hits, int n) {
        for (int i = 1; i < n; i++) {
            int h = hits[i];
            int k = i - 1;
            while (k >= 0 && compareListed(hits[k], h) > 0) {
                hits[k + 1] = hits[k];
                k--;
            }
            hits[k + 1] = h;
        }
    }

    /// Returns the shoves between the body `eid` of half width `half`
    /// and height `height` at `x`, `y`, `z` and each body it overlaps
    /// whose id is below `hi`. Each shove `[id dx dz]` moves this body
    /// by dx dz and the other body the opposite way. The shoves are
    /// not summed.
    public static Object shoves(
            PushGrid g,
            double x,
            double y,
            double z,
            double half,
            double height,
            long eid,
            long hi
    ) {
        return g.shoved(x, y, z, half, height, eid, hi);
    }

    private Object shoved(
            double x,
            double y,
            double z,
            double half,
            double height,
            long eid,
            long hi
    ) {
        int[] hits = overlapping(x, y, z, half, height, eid, hi);
        int n = hits.length;
        if (n == 0) return PersistentVector.EMPTY;
        Object[] acc = new Object[n];
        int k = 0;
        for (int j : hits) {
            double dx = x - xs[j], dz = z - zs[j];
            double m = Math.max(Math.abs(dx), Math.abs(dz));
            if (m >= THRESHOLD) {
                double s = Math.sqrt(m);
                double p = Math.min(1.0, 1.0 / s);
                acc[k++] = RT.vector(eids[j], dx / s * p * STRENGTH, dz / s * p * STRENGTH);
            }
        }
        return vector(acc, k);
    }

    /// Returns the ids of the bodies whose boxes overlap the box of
    /// the body `eid` of half width `half` and height `height` at
    /// `x`, `y`, `z`.
    public static Object touching(
            PushGrid g,
            double x,
            double y,
            double z,
            double half,
            double height,
            long eid
    ) {
        return g.touched(x, y, z, half, height, eid);
    }

    private Object touched(
            double x,
            double y,
            double z,
            double half,
            double height,
            long eid
    ) {
        int[] hits = overlapping(x, y, z, half, height, eid, Long.MAX_VALUE);
        int n = hits.length;
        Object[] acc = new Object[n];
        for (int i = 0; i < n; i++) acc[i] = eids[hits[i]];
        return vector(acc, n);
    }

    private static Object vector(Object[] a, int n) {
        if (n == 0) return PersistentVector.EMPTY;
        if (n < a.length) a = Arrays.copyOf(a, n);
        return LazilyPersistentVector.createOwning(a);
    }

    /// Returns the number of bodies of lower id that body `s` could
    /// meet in a tick in which no body moves further than half of
    /// `far` along x or z, then the indices of such bodies of higher
    /// id. It reads the places the bodies had when the grid was
    /// pinned.
    int[] near(int s, double far) {
        double x = px[s], z = pz[s], r = ph[s] + widest + far;
        long x0 = (long) Math.floor(x - r), x1 = (long) Math.floor(x + r);
        long z0 = (long) Math.floor(z - r), z1 = (long) Math.floor(z + r);
        int[] acc = new int[5];
        int n = 1;
        for (long bx = x0; bx <= x1; bx++) {
            for (long bz = z0; bz <= z1; bz++) {
                for (int j = head(key(bx, bz)); j >= 0; j = next[j]) {
                    double rj = ph[s] + ph[j] + far;
                    if (j != s && Math.abs(px[j] - x) < rj && Math.abs(pz[j] - z) < rj) {
                        if (j < s) {
                            acc[0]++;
                        } else {
                            if (n == acc.length) {
                                acc = Arrays.copyOf(acc, n * 2);
                            }
                            acc[n++] = j;
                        }
                    }
                }
            }
        }
        return Arrays.copyOf(acc, n);
    }

    int bodies() {
        return eids.length;
    }

    /// Returns the bodies of each push cell of four by four columns,
    /// by the key of the cell.
    @Override
    public Set<Map.Entry<Long, PushCell>> entrySet() {
        Map<Long, int[]> in = new HashMap<>();
        for (int i = 0; i < eids.length; i++) {
            long k = key(cell(xs[i]), cell(zs[i]));
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
        double[] h = new double[n], t = new double[n];
        double[] x = new double[n], y = new double[n], z = new double[n];
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
