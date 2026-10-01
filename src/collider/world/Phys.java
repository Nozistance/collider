package collider.world;

import collider.V3;
import java.util.Arrays;

/// Collision of a moving box against block boxes. The block tables
/// that the methods take are indexed by block state. `kinds` holds
/// the `Collision` kind of each state. `cube` is true for a full cube.
/// `shapes` holds the boxes of each other state, six doubles each,
/// in blocks. `bottom` and `flags` are the body as `Collision.shape`
/// takes it.
public final class Phys {

    private static final double EPS = 1.0E-7;

    private static final double EQUAL_SLACK = (double) 1.0E-5F;

    private static final int MIN_Y = -64;

    private static final int MAX_Y = 319;

    private static final ThreadLocal<double[]> BUF =
            ThreadLocal.withInitial(() -> new double[1536]);

    /// Returns `d` clamped so that the box `ebox` moving by `d` along
    /// `axis` stops on the face of the nearest of the `n` boxes in
    /// `a`. Each box takes six doubles.
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

    /// Writes into `out` the clamped x, y, z motion of `box` with the
    /// velocity `vx`, `vy`, `vz` against the `n` boxes in `a`. Y goes
    /// first and the wider of x and z goes last.
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

    private static double[] shapeOf(int st, byte[] kinds,
            boolean[] cube, Object[] shapes, long x, long y, long z,
            double bottom, int flags) {
        if (st <= 0) return null;
        if (st < cube.length && cube[st]) return Collision.CUBE;
        return Collision.kind(kinds, st) == Collision.PLAIN
            ? Collision.boxes(shapes, st)
            : Collision.shape(shapes, kinds, st, (int) x, (int) y,
                              (int) z, bottom, flags);
    }

    private static int putBoxes(double[] a, int n, double[] s, long x,
            long y, long z) {
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

    private static int putBlock(double[] a, int n, int st,
            byte[] kinds, boolean[] cube, Object[] shapes, long x,
            long y, long z, double bottom, int flags) {
        if (st <= 0) return n;
        if (st < cube.length && cube[st]) {
            int o = n * 6;
            a[o] = x;
            a[o + 1] = y;
            a[o + 2] = z;
            a[o + 3] = x + 1.0;
            a[o + 4] = y + 1.0;
            a[o + 5] = z + 1.0;
            return n + 1;
        }
        double[] s = shapeOf(st, kinds, cube, shapes, x, y, z, bottom,
                             flags);
        return s == null ? n : putBoxes(a, n, s, x, y, z);
    }

    private static final class Heights {
        final YCoords ys;
        final double base;
        final float step, skip;
        float[] h = new float[16];
        int k;

        Heights(YCoords ys, double base, float step, float skip) {
            this.ys = ys;
            this.base = base;
            this.step = step;
            this.skip = skip;
        }

        void add(double[] cs, long y) {
            for (double c : cs) {
                float r = (float) ((c + y) - base);
                if (r < 0.0F || r == skip) continue;
                if (r > step) return;
                if (k == h.length) h = Arrays.copyOf(h, 2 * k);
                h[k++] = r;
            }
        }
    }

    /// Returns the boxes of the blocks in `chunks` that the box
    /// `ebox` meets on its way by `vx`, `vy`, `vz`, one block
    /// lower included. The result stays valid until the next sweep
    /// of the caller.
    public static Sweep sweep(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, double[] ebox,
            double vx, double vy, double vz, double bottom, int flags) {
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
                    n = putBlock(a, n, st, kinds, cube, shapes,
                                 x, y, z, bottom, flags);
                }
            }
        }
        return new Sweep(a, n);
    }

    private static boolean meets(double[] a, int from, int to,
            double[] box) {
        for (int i = from; i < to; i++) {
            if (overlaps(a, 6 * i, box)) return true;
        }
        return false;
    }

    private static Sweep colliders(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, double[] ebox,
            double bottom, int flags, Heights hs) {
        long x1 = loBound(ebox[0], 0.0), x2 = hiBound(ebox[3], 0.0);
        long y1 = Math.max(MIN_Y, loBound(ebox[1], 0.0) - 1);
        long y2 = Math.min(MAX_Y, hiBound(ebox[4], 0.0));
        long z1 = loBound(ebox[2], 0.0), z2 = hiBound(ebox[5], 0.0);
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
                    double[] b = shapeOf(st, kinds, cube, shapes, x, y,
                                         z, bottom, flags);
                    if (b == null) continue;
                    int m = putBoxes(a, n, b, x, y, z);
                    if (meets(a, n, m, ebox)) {
                        hs.add(Collision.ys(hs.ys, st, b), y);
                        n = m;
                    }
                }
            }
        }
        return new Sweep(a, n);
    }

    /// Returns true when no cell a body box of half width `half` and
    /// that height touches, less the fluid margin, holds water or lava.
    public static boolean dry(ChunkIndex chunks, BlockTables t, double x,
            double y, double z, double half, double height) {
        double a = half - 0.001;
        long x0 = (long) Math.floor(x - a), x1 = (long) Math.ceil(x + a);
        long y0 = (long) Math.floor(y + 0.001);
        long y1 = (long) Math.ceil((y + height) - 0.001);
        long z0 = (long) Math.floor(z - a), z1 = (long) Math.ceil(z + a);
        long ckey = -1;
        Section s = null;
        for (long cx = x0; cx < x1; cx++) {
            for (long cy = Math.max(y0, MIN_Y); cy < y1 && cy <= MAX_Y;
                 cy++) {
                for (long cz = z0; cz < z1; cz++) {
                    long k = sectionKey(cx, cy, cz);
                    if (k != ckey) {
                        s = wetSection(chunks, t, cx, cy, cz);
                        ckey = k;
                    }
                    if (s == null) continue;
                    int st = s.block(
                            (int) (((cy & 15) << 8) | ((cz & 15) << 4)
                                   | (cx & 15)));
                    if (st > 0 && (Block.liquid(t, st)
                                   || Block.waterlogged(t, st))) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private static Section wetSection(ChunkIndex chunks, BlockTables t,
            long x, long y, long z) {
        Section s = Chunk.sectionAt(chunks, (int) x, (int) y, (int) z);
        return s != null && (s.holds(t.liquid()) || s.holds(t.waterlogged()))
            ? s : null;
    }

    private static boolean near(ChunkIndex chunks, byte[] bits,
            long[] span) {
        return near(chunks, bits, span[0], span[1], span[2], span[3],
                    span[4], span[5]);
    }

    private static boolean near(ChunkIndex chunks, byte[] bits, long x0,
            long x1, long y0, long y1, long z0, long z1) {
        for (long sx = x0 >> 4; sx <= (x1 - 1) >> 4; sx++) {
            for (long sy = y0 >> 4; sy <= (y1 - 1) >> 4; sy++) {
                for (long sz = z0 >> 4; sz <= (z1 - 1) >> 4; sz++) {
                    Section s = Chunk.sectionAt(chunks, (int) sx << 4,
                            (int) sy << 4, (int) sz << 4);
                    if (s != null && s.holds(bits)) return true;
                }
            }
        }
        return false;
    }

    /// Returns true when no section within a block of the box of half
    /// width `half` and `height` at `x y z` holds a state of `bits`.
    public static boolean cool(ChunkIndex chunks, byte[] bits, double x,
                               double y, double z, double half,
                               double height) {
        long y0 = Math.max(MIN_Y, (long) Math.floor(y) - 1);
        long y1 = Math.min(MAX_Y + 1, (long) Math.floor(y + height) + 2);
        return !near(chunks, bits, (long) Math.floor(x - half) - 1,
                     (long) Math.floor(x + half) + 2, y0, y1,
                     (long) Math.floor(z - half) - 1,
                     (long) Math.floor(z + half) + 2);
    }

    /// Returns the bits of `bits` of each cell from `outer` touches,
    /// each span `{x0 x1 y0 y1 z0 z1}` with the ends left out, and
    /// the bits of `inner` shifted by two for lava in a cell of
    /// `inner`. Bit 1 is fire, 2 lava.
    public static long burns(ChunkIndex chunks, byte[] bits,
                             long[] outer, long[] inner) {
        if (!near(chunks, bits, outer)) return 0;
        long acc = 0;
        for (long x = outer[0]; x < outer[1] && acc != 7; x++) {
            boolean xin = x >= inner[0] && x < inner[1];
            for (long y = outer[2]; y < outer[3] && acc != 7; y++) {
                boolean yin = xin && y >= inner[2] && y < inner[3];
                for (long z = outer[4]; z < outer[5] && acc != 7; z++) {
                    int st = Chunk.blockAt(chunks, (int) x, (int) y,
                                           (int) z);
                    long b = st >= 0 && st < bits.length ? bits[st] : 0;
                    acc |= b;
                    if ((b & 2) != 0 && yin && z >= inner[4]
                            && z < inner[5]) {
                        acc |= 4;
                    }
                }
            }
        }
        return acc;
    }

    private static boolean overlaps(double[] a, int o, double[] box) {
        return Collision.meets(a, o, box);
    }

    /// Returns true when a body with half width `half` and height
    /// `height` standing at `x`, `y`, `z` meets no block.
    public static boolean free(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, double x, double y,
            double z, double half, double height, double bottom,
            int flags) {
        double[] box = {x - half, y, z - half,
                        x + half, y + height, z + half};
        return freeBox(chunks, kinds, cube, shapes, box, bottom, flags);
    }

    /// Returns true when the box `box`, six doubles in blocks, meets
    /// no block. `bottom` is the foot of the body it belongs to.
    public static boolean freeBox(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, double[] box, double bottom,
            int flags) {
        Sweep sw = sweep(chunks, kinds, cube, shapes, box, 0.0, 0.0, 0.0,
                         bottom, flags);
        double[] a = sw.a();
        for (int i = 0; i < sw.n(); i++) {
            if (overlaps(a, 6 * i, box)) return false;
        }
        return true;
    }

    /// Returns the block x, y, z that a body with half width `half`
    /// standing at `x`, `y`, `z` rests on, or null when none. The
    /// block under the body whose centre is nearest wins. A tie goes
    /// to the last in the order of y, z and x.
    public static long[] support(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, double x, double y,
            double z, double half, int flags) {
        double[] box = {x - half, y - 1.0E-6, z - half,
                        x + half, y, z + half};
        Sweep sw = sweep(chunks, kinds, cube, shapes, box, 0.0, 0.0, 0.0,
                         y, flags);
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

    private static double[] towards(double[] b, double x, double y,
            double z) {
        double[] e = b.clone();
        e[x < 0.0 ? 0 : 3] += x;
        e[y < 0.0 ? 1 : 4] += y;
        e[z < 0.0 ? 2 : 5] += z;
        return e;
    }

    private static void stepUp(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, YCoords ys, double[] box0,
            double[] out, double vx, double vz, float step,
            boolean landed, int flags) {
        double[] g = box0.clone();
        if (landed) shift(g, 0.0, out[1], 0.0);
        double[] up = towards(g, vx, step, vz);
        if (!landed) up[1] += (double) -1.0E-5F;
        Heights hs = new Heights(ys, g[1], step, (float) out[1]);
        Sweep sw = colliders(chunks, kinds, cube, shapes, up, box0[1],
                             flags, hs);
        float[] h = hs.h;
        Arrays.sort(h, 0, hs.k);
        double[] s = new double[3];
        double far = out[0] * out[0] + out[2] * out[2];
        for (int i = 0; i < hs.k; i++) {
            if (i > 0 && h[i] == h[i - 1]) continue;
            clampAxes(sw.a(), sw.n(), g, vx, h[i], vz, s);
            if (s[0] * s[0] + s[2] * s[2] > far) {
                out[0] = s[0];
                out[1] = s[1] - (box0[1] - g[1]);
                out[2] = s[2];
                return;
            }
        }
    }

    /// Returns the move of a body with half width `half` and
    /// height `height` from `px`, `py`, `pz` by the velocity `vx`,
    /// `vy`, `vz`. The blocks it meets stop it. A body held back
    /// sideways that lands or stood on the ground, as `ground`
    /// tells, climbs to the lowest y coordinate of the shapes in
    /// `ys` up to `step` that takes it further. A move under 1.0E-7 squared that the
    /// blocks cut short leaves it where it was, as Entity.move.
    public static Move move(ChunkIndex chunks, byte[] kinds,
            boolean[] cube, Object[] shapes, double px, double py,
            double pz, double vx, double vy, double vz, double half,
            double height, double step, boolean ground, int flags,
            YCoords ys) {
        double[] box0 = {px - half, py, pz - half,
                         px + half, py + height, pz + half};
        Sweep sw = sweep(chunks, kinds, cube, shapes, box0, vx, vy, vz,
                         py, flags);
        double[] out = new double[3];
        clampAxes(sw.a(), sw.n(), box0, vx, vy, vz, out);
        boolean landed = out[1] != vy && vy < 0.0;
        if ((float) step > 0.0F && (landed || ground)
                && (out[0] != vx || out[2] != vz)) {
            stepUp(chunks, kinds, cube, shapes, ys, box0, out, vx, vz,
                   (float) step, landed, flags);
        }
        double dx = out[0], dy = out[1], dz = out[2];
        boolean hitY = dy != vy;
        double moved = dx * dx + dy * dy + dz * dz;
        double asked = vx * vx + vy * vy + vz * vz;
        boolean kept = moved > 1.0E-7 || asked - moved < 1.0E-7;
        V3 to = kept ? new V3(px + dx, py + dy, pz + dz)
                     : new V3(px, py, pz);
        return new Move(to,
                new V3(equal(dx, vx) ? vx : restituted(vx),
                       hitY ? restituted(vy) : vy,
                       equal(dz, vz) ? vz : restituted(vz)),
                hitY && vy < 0.0, dy);
    }
}
