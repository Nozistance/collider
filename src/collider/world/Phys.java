package collider.world;

import collider.V3;

/// Collision of a moving box against block boxes, one axis at a
/// time.
///
/// The block tables that the methods take are indexed by block
/// state: `solid` is true for a state that stops a body, `cube` is
/// true for a full cube, and `shapes` holds for each other state its
/// boxes as 6 doubles each, in blocks.
public final class Phys {

    private static final double EPS = 1.0E-7;

    private static final double EQUAL_SLACK = (double) 1.0E-5F;

    private static final int MIN_Y = -64;

    private static final int MAX_Y = 319;

    private static final ThreadLocal<double[]> BUF =
            ThreadLocal.withInitial(() -> new double[1536]);

    /// Returns `d` clamped so the box `ebox` moving by `d` along
    /// `axis` stops on the face of the nearest of the `n` boxes in
    /// `a`, each stored as 6 consecutive doubles (min x, y, z, max
    /// x, y, z).
    public static double clampAll(double[] a, int n, double[] ebox,
            int eo, int axis, double d) {
        int p1 = (axis == 0) ? 1 : 0;
        int q1 = (axis == 2) ? 1 : 2;
        int p2 = p1 + 3, q2 = q1 + 3;
        double ep1 = ebox[eo + p1], ep2 = ebox[eo + p2];
        double eq1 = ebox[eo + q1], eq2 = ebox[eo + q2];
        double elo = ebox[eo + axis], ehi = ebox[eo + axis + 3];
        for (int i = 0; i < n; i++) {
            if (Math.abs(d) < EPS) return 0.0;
            int o = i * 6;
            if (ep1 + EPS < a[o + p2] && ep2 - EPS >= a[o + p1]
                && eq1 + EPS < a[o + q2]
                && eq2 - EPS >= a[o + q1]) {
                double blo = a[o + axis], bhi = a[o + axis + 3];
                if (d > 0.0 && ehi - EPS < blo) {
                    double m = blo - ehi;
                    if (m < d) d = m;
                } else if (d < 0.0 && elo + EPS >= bhi) {
                    double m = bhi - elo;
                    if (m > d) d = m;
                }
            }
        }
        return d;
    }

    /// Writes into `out` the clamped x, y, z motion of `box` through
    /// the velocity `vx`, `vy`, `vz` against the `n` boxes in `a`,
    /// resolving y first and then the wider of x and z last.
    public static void clampAxes(double[] a, int n, double[] box,
            double vx, double vy, double vz, double[] out) {
        double b0 = box[0], b1 = box[1], b2 = box[2];
        double b3 = box[3], b4 = box[4], b5 = box[5];
        double[] e = {b0, b1, b2, b3, b4, b5};
        double dy = clampAll(a, n, e, 0, 1, vy);
        e[1] = b1 + dy;
        e[4] = b4 + dy;
        double dx, dz;
        if (Math.abs(vx) < Math.abs(vz)) {
            dz = clampAll(a, n, e, 0, 2, vz);
            e[2] = b2 + dz;
            e[5] = b5 + dz;
            dx = clampAll(a, n, e, 0, 0, vx);
        } else {
            dx = clampAll(a, n, e, 0, 0, vx);
            e[0] = b0 + dx;
            e[3] = b3 + dx;
            dz = clampAll(a, n, e, 0, 2, vz);
        }
        out[0] = dx;
        out[1] = dy;
        out[2] = dz;
    }

    private static double[] buffer(long need) {
        double[] b = BUF.get();
        if (b.length >= need) return b;
        double[] nb = new double[(int) (2 * need)];
        BUF.set(nb);
        return nb;
    }

    private static long loBound(double c, double v) {
        return (long) Math.floor(c + Math.min(0.0, v) - EPS);
    }

    private static long hiBound(double c, double v) {
        return (long) Math.floor(c + Math.max(0.0, v) + EPS);
    }

    private static long sectionKey(long x, long y, long z) {
        long id = ((x >> 4) & 0xFFFFFFFFL) << 32 | ((z >> 4) & 0xFFFFFFFFL);
        return (id << 5) | ((y >> 4) + 4);
    }

    private static int putBlock(double[] a, int n, int st,
            boolean[] solid, boolean[] cube, Object[] shapes,
            long x, long y, long z) {
        if (st <= 0 || st >= solid.length || !solid[st]) return n;
        if (!cube[st]) {
            double[] s = (double[]) shapes[st];
            for (int k = 0; k < s.length; k += 6) {
                int o = n * 6;
                a[o] = x + s[k];
                a[o + 1] = y + s[k + 1];
                a[o + 2] = z + s[k + 2];
                a[o + 3] = x + s[k + 3];
                a[o + 4] = y + s[k + 4];
                a[o + 5] = z + s[k + 5];
                n++;
            }
            return n;
        }
        int o = n * 6;
        a[o] = x;
        a[o + 1] = y;
        a[o + 2] = z;
        a[o + 3] = x + 1.0;
        a[o + 4] = y + 1.0;
        a[o + 5] = z + 1.0;
        return n + 1;
    }

    /// Returns the boxes of the blocks in `chunks` that the box
    /// `ebox` meets on its way by `vx`, `vy`, `vz`, one block
    /// lower included. The boxes live in a buffer of the thread,
    /// good until its next sweep.
    public static Sweep sweep(ChunkIndex chunks, boolean[] solid,
            boolean[] cube, Object[] shapes, double[] ebox,
            double vx, double vy, double vz) {
        long x1 = loBound(ebox[0], vx), x2 = hiBound(ebox[3], vx);
        long y1 = Math.max(MIN_Y, loBound(ebox[1], vy) - 1);
        long y2 = Math.min(MAX_Y, hiBound(ebox[4], vy));
        long z1 = loBound(ebox[2], vz), z2 = hiBound(ebox[5], vz);
        long cells = (x2 - x1 + 1) * (Math.max(y1, y2) - y1 + 1)
                * (z2 - z1 + 1);
        double[] a = buffer(96 * Math.max(1, cells));
        int n = 0;
        long ckey = -1;
        Section s = null;
        for (long x = x1; x <= x2; x++) {
            for (long z = z1; z <= z2; z++) {
                for (long y = y1; y <= y2; y++) {
                    long k = sectionKey(x, y, z);
                    if (k != ckey) {
                        s = Chunk.sectionAt(chunks, (int) x, (int) y,
                                            (int) z);
                        ckey = k;
                    }
                    int st = s == null ? 0 : s.block(
                            (int) (((y & 15) << 8) | ((z & 15) << 4)
                                   | (x & 15)));
                    n = putBlock(a, n, st, solid, cube, shapes,
                                 x, y, z);
                }
            }
        }
        return new Sweep(a, n);
    }

    private static boolean overlaps(double[] a, int o, double[] box) {
        return a[o + 3] > box[0] && box[3] > a[o]
            && a[o + 4] > box[1] && box[4] > a[o + 1]
            && a[o + 5] > box[2] && box[5] > a[o + 2];
    }

    /// Returns true when a body with half width `half` and height
    /// `height` standing at `x`, `y`, `z` meets no block.
    public static boolean free(ChunkIndex chunks, boolean[] solid,
            boolean[] cube, Object[] shapes, double x, double y,
            double z, double half, double height) {
        double[] box = {x - half, y, z - half,
                        x + half, y + height, z + half};
        Sweep sw = sweep(chunks, solid, cube, shapes, box, 0.0, 0.0, 0.0);
        double[] a = sw.a();
        for (int i = 0; i < sw.n(); i++) {
            if (overlaps(a, 6 * i, box)) return false;
        }
        return true;
    }

    /// Returns the block x, y, z that a body with half width `half`
    /// standing at `x`, `y`, `z` rests on, or null when none. Of the
    /// blocks under the body, the one whose centre is nearest wins;
    /// a tie goes to the last in the order of y, z and x.
    public static long[] support(ChunkIndex chunks, boolean[] solid,
            boolean[] cube, Object[] shapes, double x, double y,
            double z, double half) {
        double[] box = {x - half, y - 1.0E-6, z - half,
                        x + half, y, z + half};
        Sweep sw = sweep(chunks, solid, cube, shapes, box, 0.0, 0.0, 0.0);
        double[] a = sw.a();
        long[] best = null;
        double bd = Double.MAX_VALUE;
        for (int i = 0; i < sw.n(); i++) {
            int o = 6 * i;
            long bx = (long) Math.floor(a[o]);
            long by = (long) Math.floor(a[o + 1]);
            long bz = (long) Math.floor(a[o + 2]);
            double dx = (bx + 0.5) - x;
            double dy = (by + 0.5) - y;
            double dz = (bz + 0.5) - z;
            double d = dx * dx + dy * dy + dz * dz;
            if (overlaps(a, o, box) && (d < bd || (d == bd
                    && later(bx, by, bz, best)))) {
                best = new long[] {bx, by, bz};
                bd = d;
            }
        }
        return best;
    }

    private static boolean later(long bx, long by, long bz, long[] best) {
        if (best == null) return true;
        if (best[1] != by) return best[1] < by;
        if (best[2] != bz) return best[2] < bz;
        return best[0] < bx;
    }

    private static boolean equal(double a, double b) {
        return Math.abs(b - a) < EQUAL_SLACK;
    }

    private static double restituted(double v) {
        return -v * 0.0;
    }

    private static void shift(double[] e, double dx, double dy,
            double dz) {
        e[0] += dx;
        e[3] += dx;
        e[1] += dy;
        e[4] += dy;
        e[2] += dz;
        e[5] += dz;
    }

    private static void stepUp(ChunkIndex chunks, boolean[] solid,
            boolean[] cube, Object[] shapes, double[] box0,
            double[] out, double vx, double vz, double step) {
        double dy0 = out[1];
        double[] e = box0.clone();
        e[1] += dy0;
        e[4] += dy0;
        Sweep sw = sweep(chunks, solid, cube, shapes, e, vx, step, vz);
        double[] s = new double[3];
        clampAxes(sw.a(), sw.n(), e, vx, step, vz, s);
        shift(e, s[0], s[1], s[2]);
        double drop = clampAll(sw.a(), sw.n(), e, 0, 1, -s[1]);
        double sx = s[0], sz = s[2], ox = out[0], oz = out[2];
        if (sx * sx + sz * sz > ox * ox + oz * oz) {
            out[0] = sx;
            out[1] = dy0 + s[1] + drop;
            out[2] = sz;
        }
    }

    /// Returns the move of a body with half width `half` and height
    /// `height` from `px`, `py`, `pz` by the velocity `vx`, `vy`,
    /// `vz`. The blocks it meets stop it; a body that lands and is
    /// held back sideways climbs up to `step` when that takes it
    /// further.
    public static Move move(ChunkIndex chunks, boolean[] solid,
            boolean[] cube, Object[] shapes, double px, double py,
            double pz, double vx, double vy, double vz, double half,
            double height, double step) {
        double[] box0 = {px - half, py, pz - half,
                         px + half, py + height, pz + half};
        Sweep sw = sweep(chunks, solid, cube, shapes, box0, vx, vy, vz);
        double[] out = new double[3];
        clampAxes(sw.a(), sw.n(), box0, vx, vy, vz, out);
        boolean hitY = out[1] != vy;
        if (step > 0.0 && hitY && vy < 0.0
                && (out[0] != vx || out[2] != vz)) {
            stepUp(chunks, solid, cube, shapes, box0, out, vx, vz, step);
        }
        double dx = out[0], dy = out[1], dz = out[2];
        return new Move(new V3(px + dx, py + dy, pz + dz),
                new V3(equal(dx, vx) ? vx : restituted(vx),
                       hitY ? restituted(vy) : vy,
                       equal(dz, vz) ? vz : restituted(vz)),
                hitY && vy < 0.0);
    }
}
