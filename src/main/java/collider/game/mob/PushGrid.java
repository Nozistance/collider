package collider.game.mob;

import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import collider.world.LongIntMap;
import java.util.Arrays;

/// The bodies of one island by the block column they stand in, for
/// the shoves between overlapping bodies. It changes in place as the
/// bodies move, so one island steps with one grid. Many readers may
/// share it while no body moves. A body meets the others by entity
/// section, and in each section in the order they came into it.
/// Shoves from the bodies that stepped before come in the order they
/// stepped, by id.
public final class PushGrid {

    private static final double STRENGTH = 0.05F;

    private static final double THRESHOLD = 0.01F;

    private static final byte NEVER = 1, PUSH_OTHER_TEAMS = 2, PUSH_OWN_TEAM = 3;

    private final long[] eids;
    private final double[] halfs, heights, xs, ys, zs;
    private final long[] came, ranks;
    private final boolean[] steps;
    private final int[] next;
    private final LongIntMap heads;
    private double widest, slack;
    private double[] px, pz, ph;
    private byte[] rules;
    private int[] teams;

    /// The ids eids ascend. Every other array holds the body of
    /// eids at the same index.
    PushGrid(
            long[] eids,
            double[] halfs,
            double[] heights,
            double[] xs,
            double[] ys,
            double[] zs,
            long[] came,
            long[] ranks,
            boolean[] steps
    ) {
        this.eids = eids;
        this.came = came;
        this.ranks = ranks;
        this.steps = steps;
        this.halfs = halfs;
        this.heights = heights;
        this.xs = xs;
        this.ys = ys;
        this.zs = zs;
        int n = eids.length;
        this.next = new int[n];
        this.heads = new LongIntMap(2 * n);
        for (int i = 0; i < n; i++) {
            widest = Math.max(widest, halfs[i]);
            link(i);
        }
    }

    static long key(long cx, long cz) {
        return ((cx & 0xFFFFFFFFL) << 32) | (cz & 0xFFFFFFFFL);
    }

    /// Returns the push cell of four columns that holds coordinate c.
    static long cell(double c) {
        return Math.floorDiv((long) Math.floor(c), 4);
    }

    private static long column(double x, double z) {
        return key((long) Math.floor(x), (long) Math.floor(z));
    }

    private void link(int s) {
        next[s] = heads.put(column(xs[s], zs[s]), s);
    }

    private void unlink(int s) {
        long k = column(xs[s], zs[s]);
        int p = heads.get(k);
        if (p == s) {
            heads.put(k, next[s]);
            return;
        }
        while (next[p] != s) {
            p = next[p];
        }
        next[p] = next[s];
    }

    /// Returns the index of body eid in g, or -1 for a body that
    /// is not in it.
    public static long slot(PushGrid g, long eid) {
        int s = Arrays.binarySearch(g.eids, eid);
        return s < 0 ? -1 : s;
    }

    /// Moves body eid of g and returns g. The tick c and the
    /// rank r place it among the bodies of its section.
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
        int s = Arrays.binarySearch(g.eids, eid);
        g.came[s] = c;
        g.ranks[s] = r;
        boolean same = g.slack > 0.0 || column(x, z) == column(g.xs[s], g.zs[s]);
        if (!same) {
            g.unlink(s);
        }
        g.halfs[s] = half;
        g.heights[s] = height;
        g.xs[s] = x;
        g.ys[s] = y;
        g.zs[s] = z;
        if (g.slack == 0.0) {
            g.widest = Math.max(g.widest, half);
        }
        if (!same) {
            g.link(s);
        }
        return g;
    }

    /// Keeps each body of grid g in the column it stands in now,
    /// for the moves of a tick in which no body moves further than
    /// reach along x or z, and no body changes its box. Returns g,
    /// which many readers may share while the bodies move.
    public static PushGrid pinned(PushGrid g, double reach) {
        g.slack = reach;
        g.px = g.xs.clone();
        g.pz = g.zs.clone();
        g.ph = g.halfs.clone();
        return g;
    }

    /// Returns g, in which each body meets the others as the
    /// collision rule rules of its team teams lets it, by index.
    /// A body of no team is in team -1 with rule 0.
    public static PushGrid teamed(PushGrid g, byte[] rules, int[] teams) {
        g.rules = rules;
        g.teams = teams;
        return g;
    }

    private boolean meets(int a, int b) {
        byte ra = rules[a], rb = rules[b];
        if (ra == NEVER || rb == NEVER) {
            return false;
        }
        boolean same = teams[a] >= 0 && teams[a] == teams[b];
        if ((ra == PUSH_OWN_TEAM || rb == PUSH_OWN_TEAM) && same) {
            return false;
        }
        return ra != PUSH_OTHER_TEAMS && rb != PUSH_OTHER_TEAMS || same;
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
        int s = rules == null ? -1 : Arrays.binarySearch(eids, eid);
        int[] hits = new int[16];
        int n = 0;
        for (long bx = x0; bx <= x1; bx++) {
            long dx = Math.floorDiv(bx, 4) - cx;
            if (!pinned && (dx < -1 || dx > 1)) {
                continue;
            }
            for (long bz = z0; bz <= z1; bz++) {
                long dz = Math.floorDiv(bz, 4) - cz;
                if (!pinned && (dz < -1 || dz > 1)) {
                    continue;
                }
                for (int j = heads.get(key(bx, bz)); j >= 0; j = next[j]) {
                    long o = eids[j];
                    if (o == eid || o >= hi) {
                        continue;
                    }
                    if (hi != Long.MAX_VALUE && !steps[j]) {
                        continue;
                    }
                    if (s >= 0 && !meets(s, j)) {
                        continue;
                    }
                    double rj = half + halfs[j];
                    double oy = ys[j];
                    if (Math.abs(xs[j] - x) < rj
                            && Math.abs(zs[j] - z) < rj
                            && oy < y + height
                            && oy + heights[j] > y) {
                        long ox = cell(xs[j]) - cx, oz = cell(zs[j]) - cz;
                        if (ox < -1 || ox > 1 || oz < -1 || oz > 1) {
                            continue;
                        }
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
        if (c != 0) {
            return c;
        }
        c = Long.compare(section(zs[a]) & 0x3FFFFFL, section(zs[b]) & 0x3FFFFFL);
        if (c != 0) {
            return c;
        }
        c = Long.compare(section(ys[a]) & 0xFFFFFL, section(ys[b]) & 0xFFFFFL);
        if (c != 0) {
            return c;
        }
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

    /// Returns the shoves between the body eid of half width half
    /// and height height at x, y, z and each body it overlaps
    /// whose id is below hi. Below a bound hi, only the bodies
    /// that step this tick count. Each shove [id dx dz] moves this
    /// body by dx dz and the other body the opposite way. The shoves
    /// are not summed.
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
        int[] hits = g.overlapping(x, y, z, half, height, eid, hi);
        int n = hits.length;
        if (n == 0) {
            return PersistentVector.EMPTY;
        }
        Object[] acc = new Object[n];
        int k = 0;
        for (int j : hits) {
            double dx = x - g.xs[j], dz = z - g.zs[j];
            double m = Math.max(Math.abs(dx), Math.abs(dz));
            if (m >= THRESHOLD) {
                double s = Math.sqrt(m);
                double p = Math.min(1.0, 1.0 / s);
                acc[k++] = RT.vector(
                        g.eids[j],
                        dx / s * p * STRENGTH,
                        dz / s * p * STRENGTH
                );
            }
        }
        return vector(acc, k);
    }

    /// Returns the ids of the bodies whose boxes overlap the box of
    /// the body eid of half width half and height height at
    /// x, y, z.
    public static Object touching(
            PushGrid g,
            double x,
            double y,
            double z,
            double half,
            double height,
            long eid
    ) {
        int[] hits = g.overlapping(x, y, z, half, height, eid, Long.MAX_VALUE);
        int n = hits.length;
        Object[] acc = new Object[n];
        for (int i = 0; i < n; i++) {
            acc[i] = g.eids[hits[i]];
        }
        return vector(acc, n);
    }

    private static Object vector(Object[] a, int n) {
        if (n == 0) {
            return PersistentVector.EMPTY;
        }
        if (n < a.length) {
            a = Arrays.copyOf(a, n);
        }
        return LazilyPersistentVector.createOwning(a);
    }

    /// Returns the number of bodies of lower id that body s could
    /// meet in a tick in which no body moves further than half of
    /// far along x or z, followed by the indices of such bodies of
    /// higher id. It reads the places the bodies had when the grid
    /// was pinned.
    int[] near(int s, double far) {
        double x = px[s], z = pz[s], r = ph[s] + widest + far;
        long x0 = (long) Math.floor(x - r), x1 = (long) Math.floor(x + r);
        long z0 = (long) Math.floor(z - r), z1 = (long) Math.floor(z + r);
        int[] acc = new int[5];
        int n = 1;
        for (long bx = x0; bx <= x1; bx++) {
            for (long bz = z0; bz <= z1; bz++) {
                for (int j = heads.get(key(bx, bz)); j >= 0; j = next[j]) {
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
}
