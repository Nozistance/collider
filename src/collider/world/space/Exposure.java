package collider.world.space;

/// The cells around a blast that may block its sight, to share among
/// the bodies it reaches. Sight follows `ServerExplosion.getSeenPercent`:
/// a clip from each sample point of the body to the blast against the
/// collision boxes of the blocks.
public final class Exposure {

    /// The width of the cube of cells that an exposure keeps.
    public static final int W = 24;

    private static final int S = W + 1;

    private static final double[] CUBE = {0, 0, 0, 1, 1, 1};

    private static final double[] UNSTABLE_BOTTOM = {0, 0, 0, 1, 0.125, 1};

    private static final double[] SNOW_FALLING = {0, 0, 0, 1, 0.9F, 1};

    /// The kind of a block state whose collision shape depends on the
    /// body that looks or on the position of the block.
    public static final byte PLAIN = 0, SCAFFOLDING = 1,
        SCAFFOLDING_HANGING = 2, POWDER_SNOW = 3, OFFSET_QUARTER = 4,
        OFFSET_EIGHTH = 5;

    /// The flags of a body that looks: it descends, it falls more
    /// than 2.5 blocks, it walks on powder snow.
    public static final int DESCENDING = 1, FALLING = 2, WALKER = 4;

    private final Region rg;
    private final Object[] shapes;
    private final byte[] kinds;
    private final Craters craters;
    private final double cx, cy, cz;
    private final int ox, oy, oz;
    private int[] states;
    private byte[] cells;
    private int[] sums;
    private Exposure revised;
    private boolean clears;

    /// Returns the exposure of a blast at `cx`, `cy`, `cz` through
    /// the sections of `rg`, with nothing read yet. `shapes` holds the
    /// collision boxes by block state, six doubles each, as an empty
    /// context meets them at the origin. `kinds` holds the kind of
    /// each block state.
    public static Exposure of(Region rg, Object[] shapes, byte[] kinds,
            double cx, double cy, double cz) {
        return new Exposure(rg, shapes, kinds, null, cx, cy, cz);
    }

    private Exposure(Region rg, Object[] shapes, byte[] kinds,
            Craters craters, double cx, double cy, double cz) {
        this.rg = rg;
        this.shapes = shapes;
        this.kinds = kinds;
        this.craters = craters;
        this.cx = cx;
        this.cy = cy;
        this.cz = cz;
        this.ox = (int) Math.floor(cx) - W / 2;
        this.oy = (int) Math.floor(cy) - W / 2;
        this.oz = (int) Math.floor(cz) - W / 2;
    }

    private int state(int x, int y, int z) {
        int ix = x - ox, iy = y - oy, iz = z - oz;
        if (states != null && ix >= 0 && ix < W && iy >= 0 && iy < W
                && iz >= 0 && iz < W) {
            return states[(ix * W + iy) * W + iz];
        }
        return read(x, y, z);
    }

    private int read(int x, int y, int z) {
        if (craters != null) {
            int s = craters.state(x, y, z);
            if (s >= 0) return s;
        }
        return Rays.readBlock(rg.grid(), rg.cx0(), rg.cz0(), rg.sy0(),
                              rg.ncx(), rg.ncz(), rg.nsy(), x, y, z);
    }

    private double[] boxes(int st) {
        if (st <= 0) return null;
        if (st >= shapes.length) return CUBE;
        double[] b = (double[]) shapes[st];
        return b.length == 0 ? null : b;
    }

    private int kind(int st) {
        return st > 0 && st < kinds.length ? kinds[st] : PLAIN;
    }

    private boolean mayCollide(int st) {
        return boxes(st) != null || kind(st) == POWDER_SNOW;
    }

    private static boolean above(double bottom, int y, double top) {
        return bottom > (double) y + top - 1.0E-5F;
    }

    private static long seed(int x, int z) {
        long seed = (long) (x * 3129871) ^ (long) z * 116129781L;
        seed = seed * seed * 42317861L + seed * 11L;
        return seed >> 16;
    }

    private static double offset(long bits, double max) {
        double v = ((double) ((float) (bits & 15L) / 15.0F) - 0.5) * 0.5;
        return v < -max ? -max : Math.min(v, max);
    }

    private static double[] moved(double[] b, double max, int x, int z) {
        long seed = seed(x, z);
        double dx = offset(seed, max), dz = offset(seed >> 8, max);
        double[] m = b.clone();
        for (int k = 0; k < m.length; k += 6) {
            m[k] = (b[k] + max) + dx;
            m[k + 2] = (b[k + 2] + max) + dz;
            m[k + 3] = (b[k + 3] + max) + dx;
            m[k + 5] = (b[k + 5] + max) + dz;
        }
        return m;
    }

    private double[] shape(int st, int x, int y, int z, double bottom,
            int flags) {
        switch (kind(st)) {
            case SCAFFOLDING, SCAFFOLDING_HANGING -> {
                boolean still = (flags & DESCENDING) == 0;
                if (still && above(bottom, y, 1.0)) return boxes(st);
                return kind(st) == SCAFFOLDING_HANGING
                    && above(bottom, y, 0.0) ? UNSTABLE_BOTTOM : null;
            }
            case POWDER_SNOW -> {
                if ((flags & FALLING) != 0) return SNOW_FALLING;
                return (flags & WALKER) != 0 && above(bottom, y, 1.0)
                    && (flags & DESCENDING) == 0 ? CUBE : null;
            }
            case OFFSET_QUARTER -> {
                double[] b = boxes(st);
                return b == null ? null : moved(b, 0.25F, x, z);
            }
            case OFFSET_EIGHTH -> {
                double[] b = boxes(st);
                return b == null ? null : moved(b, 0.125F, x, z);
            }
            default -> {
                return boxes(st);
            }
        }
    }

    private static int sumIndex(int ix, int iy, int iz) {
        return (ix * S + iy) * S + iz;
    }

    private void sum() {
        sums = new int[S * S * S];
        for (int ix = 0; ix < W; ix++) {
            for (int iy = 0; iy < W; iy++) {
                for (int iz = 0; iz < W; iz++) {
                    int c = cells[(ix * W + iy) * W + iz];
                    sums[sumIndex(ix + 1, iy + 1, iz + 1)] = c
                        + sums[sumIndex(ix, iy + 1, iz + 1)]
                        + sums[sumIndex(ix + 1, iy, iz + 1)]
                        + sums[sumIndex(ix + 1, iy + 1, iz)]
                        - sums[sumIndex(ix, iy, iz + 1)]
                        - sums[sumIndex(ix, iy + 1, iz)]
                        - sums[sumIndex(ix + 1, iy, iz)]
                        + sums[sumIndex(ix, iy, iz)];
                }
            }
        }
    }

    private void build() {
        int[] st = new int[W * W * W];
        cells = new byte[W * W * W];
        for (int ix = 0; ix < W; ix++) {
            for (int iy = 0; iy < W; iy++) {
                for (int iz = 0; iz < W; iz++) {
                    int i = (ix * W + iy) * W + iz;
                    st[i] = read(ox + ix, oy + iy, oz + iz);
                    cells[i] = (byte) (mayCollide(st[i]) ? 1 : 0);
                }
            }
        }
        states = st;
        sum();
    }

    private Exposure revise(Craters c) {
        if (cells == null) build();
        Exposure e = new Exposure(rg, shapes, kinds, c, cx, cy, cz);
        e.states = states.clone();
        e.cells = cells.clone();
        int[] changed = c.inCube(ox, oy, oz, W);
        e.clears = c.clears(this::mayCollide);
        for (int i = 0; i < changed.length; i += 2) {
            e.states[changed[i]] = changed[i + 1];
            e.cells[changed[i]] = (byte) (mayCollide(changed[i + 1]) ? 1 : 0);
        }
        e.sum();
        return e;
    }

    private int collidingIn(int x0, int y0, int z0, int x1, int y1,
            int z1) {
        int ax = x0 - ox, ay = y0 - oy, az = z0 - oz;
        int bx = x1 - ox + 1, by = y1 - oy + 1, bz = z1 - oz + 1;
        if (ax < 0 || ay < 0 || az < 0 || bx > W || by > W || bz > W) {
            return -1;
        }
        return sums[sumIndex(bx, by, bz)] - sums[sumIndex(ax, by, bz)]
            - sums[sumIndex(bx, ay, bz)] - sums[sumIndex(bx, by, az)]
            + sums[sumIndex(ax, ay, bz)] + sums[sumIndex(ax, by, az)]
            + sums[sumIndex(bx, ay, az)] - sums[sumIndex(ax, ay, az)];
    }

    private static int floor(double v) {
        int i = (int) v;
        return v < i ? i - 1 : i;
    }

    private static double lerp(double t, double a, double b) {
        return a + t * (b - a);
    }

    private static double frac(double v) {
        return v - Math.floor(v);
    }

    private static int sign(double v) {
        return v == 0.0 ? 0 : (v > 0.0 ? 1 : -1);
    }

    private static boolean inside(double[] b, double x, double y,
            double z) {
        for (int k = 0; k < b.length; k += 6) {
            if (x >= b[k] && x < b[k + 3] && y >= b[k + 1]
                    && y < b[k + 4] && z >= b[k + 2] && z < b[k + 5]) {
                return true;
            }
        }
        return false;
    }

    private static double clipPoint(double scale, double da, double db,
            double dc, double point, double minB, double maxB,
            double minC, double maxC, double fromA, double fromB,
            double fromC) {
        double s = (point - fromA) / da;
        double pb = fromB + s * db;
        double pc = fromC + s * dc;
        if (0.0 < s && s < scale && minB - 1.0E-7 < pb
                && pb < maxB + 1.0E-7 && minC - 1.0E-7 < pc
                && pc < maxC + 1.0E-7) return s;
        return -1.0;
    }

    private static boolean clipBoxes(double[] b, int px, int py,
            int pz, double fx, double fy, double fz, double dx,
            double dy, double dz) {
        double scale = 1.0;
        boolean hit = false;
        for (int k = 0; k < b.length; k += 6) {
            double x0 = b[k] + px, y0 = b[k + 1] + py, z0 = b[k + 2] + pz;
            double x1 = b[k + 3] + px, y1 = b[k + 4] + py;
            double z1 = b[k + 5] + pz;
            double s = -1.0;
            if (dx > 1.0E-7) {
                s = clipPoint(scale, dx, dy, dz, x0, y0, y1, z0, z1, fx, fy, fz);
            } else if (dx < -1.0E-7) {
                s = clipPoint(scale, dx, dy, dz, x1, y0, y1, z0, z1, fx, fy, fz);
            }
            if (s >= 0.0) {
                scale = s;
                hit = true;
            }
            s = -1.0;
            if (dy > 1.0E-7) {
                s = clipPoint(scale, dy, dz, dx, y0, z0, z1, x0, x1, fy, fz, fx);
            } else if (dy < -1.0E-7) {
                s = clipPoint(scale, dy, dz, dx, y1, z0, z1, x0, x1, fy, fz, fx);
            }
            if (s >= 0.0) {
                scale = s;
                hit = true;
            }
            s = -1.0;
            if (dz > 1.0E-7) {
                s = clipPoint(scale, dz, dx, dy, z0, x0, x1, y0, y1, fz, fx, fy);
            } else if (dz < -1.0E-7) {
                s = clipPoint(scale, dz, dx, dy, z1, x0, x1, y0, y1, fz, fx, fy);
            }
            if (s >= 0.0) {
                scale = s;
                hit = true;
            }
        }
        return hit;
    }

    private boolean shapeHit(int px, int py, int pz, double fx,
            double fy, double fz, double tx, double ty, double tz,
            double bottom, int flags) {
        double[] b = shape(state(px, py, pz), px, py, pz, bottom, flags);
        if (b == null) return false;
        double dx = tx - fx, dy = ty - fy, dz = tz - fz;
        if (dx * dx + dy * dy + dz * dz < 1.0E-7) return false;
        double qx = fx + dx * 0.001, qy = fy + dy * 0.001;
        double qz = fz + dz * 0.001;
        if (inside(b, qx - px, qy - py, qz - pz)) return true;
        return clipBoxes(b, px, py, pz, fx, fy, fz, dx, dy, dz);
    }

    private static final int MISS = -1, AFAR = -2;

    private int cubeIndex(int x, int y, int z) {
        int ix = x - ox, iy = y - oy, iz = z - oz;
        if (ix < 0 || ix >= W || iy < 0 || iy >= W || iz < 0 || iz >= W) {
            return AFAR;
        }
        return (ix * W + iy) * W + iz;
    }

    private boolean changed(Craters c, int i) {
        int ix = i / (W * W), iy = i / W % W, iz = i % W;
        return c.state(ox + ix, oy + iy, oz + iz) >= 0;
    }

    private int clip(double fx, double fy, double fz, double bottom,
            int flags) {
        double tx = cx, ty = cy, tz = cz;
        if (Double.compare(fx, tx) == 0 && Double.compare(fy, ty) == 0
                && Double.compare(fz, tz) == 0) return MISS;
        double toX = lerp(-1.0E-7, tx, fx), toY = lerp(-1.0E-7, ty, fy);
        double toZ = lerp(-1.0E-7, tz, fz);
        double frX = lerp(-1.0E-7, fx, tx), frY = lerp(-1.0E-7, fy, ty);
        double frZ = lerp(-1.0E-7, fz, tz);
        int bx = floor(frX), by = floor(frY), bz = floor(frZ);
        if (collidingIn(Math.min(bx, floor(toX)) - 1,
                        Math.min(by, floor(toY)) - 1,
                        Math.min(bz, floor(toZ)) - 1,
                        Math.max(bx, floor(toX)) + 1,
                        Math.max(by, floor(toY)) + 1,
                        Math.max(bz, floor(toZ)) + 1) == 0) return MISS;
        if (shapeHit(bx, by, bz, fx, fy, fz, tx, ty, tz, bottom, flags)) {
            return cubeIndex(bx, by, bz);
        }
        double dx = toX - frX, dy = toY - frY, dz = toZ - frZ;
        int sx = sign(dx), sy = sign(dy), sz = sign(dz);
        double tdx = sx == 0 ? Double.MAX_VALUE : sx / dx;
        double tdy = sy == 0 ? Double.MAX_VALUE : sy / dy;
        double tdz = sz == 0 ? Double.MAX_VALUE : sz / dz;
        double tX = tdx * (sx > 0 ? 1.0 - frac(frX) : frac(frX));
        double tY = tdy * (sy > 0 ? 1.0 - frac(frY) : frac(frY));
        double tZ = tdz * (sz > 0 ? 1.0 - frac(frZ) : frac(frZ));
        while (tX <= 1.0 || tY <= 1.0 || tZ <= 1.0) {
            if (tX < tY) {
                if (tX < tZ) {
                    bx += sx;
                    tX += tdx;
                } else {
                    bz += sz;
                    tZ += tdz;
                }
            } else if (tY < tZ) {
                by += sy;
                tY += tdy;
            } else {
                bz += sz;
                tZ += tdz;
            }
            if (shapeHit(bx, by, bz, fx, fy, fz, tx, ty, tz, bottom, flags)) {
                return cubeIndex(bx, by, bz);
            }
        }
        return MISS;
    }

    /// Returns true when the cells in `c` that the earlier blasts of
    /// the tick changed may change what a body box at `px`, `py`, `pz`
    /// with half width `half` and height `height` sees of the blast
    /// of `e`.
    public static boolean stale(Exposure e, Craters c, double px,
            double py, double pz, double half, double height) {
        double w = (float) half, h = (float) height;
        return c != null && c.anyIn(floor(Math.min(e.cx, px - w)) - 1,
                                    floor(Math.min(e.cy, py)) - 1,
                                    floor(Math.min(e.cz, pz - w)) - 1,
                                    floor(Math.max(e.cx, px + w)) + 1,
                                    floor(Math.max(e.cy, py + h)) + 1,
                                    floor(Math.max(e.cz, pz + w)) + 1);
    }

    /// Returns the share from 0.0 to 1.0 of the sample points of a body
    /// box at `px`, `py`, `pz` with half width `half` and height
    /// `height` that see the blast of `e`. `flags` tell how the body
    /// meets the blocks whose shape depends on it. The cells in `c`
    /// that the earlier blasts of the tick changed count as they stand
    /// now.
    public static double density(Exposure e, Craters c, double px,
            double py, double pz, double half, double height,
            int flags) {
        return densityNow(e, c, null, px, py, pz, half, height, flags);
    }

    /// Returns what the sample points of a body see of the blast of
    /// `e` before the blasts of the tick, as `density` does. The first
    /// long holds the bits of the share, the rest one for each sample
    /// point: -1 when it sees the blast, else the cube index of the
    /// cell in the way, -2 for a cell out of the cube.
    public static long[] look(Exposure e, double px, double py,
            double pz, double half, double height, int flags) {
        return e.sight(null, null, px, py, pz, half, height, flags);
    }

    /// Returns what `density` does, with `look` as the body saw the
    /// blast before the blasts of the tick, or nil. A sample point
    /// keeps what it saw then when no cell in `c` could change it: the
    /// cell in its way stands, or it saw the blast and the blasts only
    /// cleared cells or left its ray alone.
    public static double densityNow(Exposure e, Craters c, long[] look,
            double px, double py, double pz, double half,
            double height, int flags) {
        if (!stale(e, c, px, py, pz, half, height)) {
            if (look != null) return Double.longBitsToDouble(look[0]);
            return Double.longBitsToDouble(
                e.sight(null, null, px, py, pz, half, height, flags)[0]);
        }
        if (e.revised == null) e.revised = e.revise(c);
        long[] now = e.revised.sight(c, look, px, py, pz, half, height,
                                     flags);
        return Double.longBitsToDouble(now[0]);
    }

    private boolean rayMet(Craters c, double fx, double fy, double fz) {
        double toX = lerp(-1.0E-7, cx, fx), toY = lerp(-1.0E-7, cy, fy);
        double toZ = lerp(-1.0E-7, cz, fz);
        double frX = lerp(-1.0E-7, fx, cx), frY = lerp(-1.0E-7, fy, cy);
        double frZ = lerp(-1.0E-7, fz, cz);
        return c.anyIn(Math.min(floor(frX), floor(toX)) - 1,
                       Math.min(floor(frY), floor(toY)) - 1,
                       Math.min(floor(frZ), floor(toZ)) - 1,
                       Math.max(floor(frX), floor(toX)) + 1,
                       Math.max(floor(frY), floor(toY)) + 1,
                       Math.max(floor(frZ), floor(toZ)) + 1);
    }

    private long[] sight(Craters c, long[] then, double px, double py,
            double pz, double half, double height, int flags) {
        if (cells == null) build();
        double w = (float) half, h = (float) height;
        double x0 = px - w, y0 = py, z0 = pz - w;
        double x1 = px + w, y1 = py + h, z1 = pz + w;
        double xs = 1.0 / ((x1 - x0) * 2.0 + 1.0);
        double ys = 1.0 / ((y1 - y0) * 2.0 + 1.0);
        double zs = 1.0 / ((z1 - z0) * 2.0 + 1.0);
        double xo = (1.0 - Math.floor(1.0 / xs) * xs) / 2.0;
        double zo = (1.0 - Math.floor(1.0 / zs) * zs) / 2.0;
        boolean clear = collidingIn(floor(Math.min(cx, x0)) - 1,
                                    floor(Math.min(cy, y0)) - 1,
                                    floor(Math.min(cz, z0)) - 1,
                                    floor(Math.max(cx, x1)) + 1,
                                    floor(Math.max(cy, y1)) + 1,
                                    floor(Math.max(cz, z1)) + 1) == 0;
        long[] out = new long[16];
        int hits = 0, count = 0;
        for (double xx = 0.0; xx <= 1.0; xx += xs) {
            for (double yy = 0.0; yy <= 1.0; yy += ys) {
                for (double zz = 0.0; zz <= 1.0; zz += zs) {
                    double x = lerp(xx, x0, x1) + xo;
                    double y = lerp(yy, y0, y1);
                    double z = lerp(zz, z0, z1) + zo;
                    int hit = clear ? MISS
                        : seen(c, then, count, x, y, z, py, flags);
                    if (count + 1 >= out.length) {
                        out = java.util.Arrays.copyOf(out, 2 * out.length);
                    }
                    out[count + 1] = hit;
                    if (hit == MISS) hits++;
                    count++;
                }
            }
        }
        out[0] = Double.doubleToRawLongBits(
            (double) ((float) hits / (float) count));
        return out;
    }

    private int seen(Craters c, long[] then, int i, double x, double y,
            double z, double bottom, int flags) {
        if (then != null) {
            int was = (int) then[i + 1];
            if (was >= 0 && !changed(c, was)) return was;
            if (was == MISS && (clears || !rayMet(c, x, y, z))) return MISS;
        }
        return clip(x, y, z, bottom, flags);
    }
}
