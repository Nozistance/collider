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

    private final Region rg;
    private final Object[] shapes;
    private final Craters craters;
    private final double cx, cy, cz;
    private final int ox, oy, oz;
    private byte[] cells;
    private int[] sums;
    private Exposure revised;

    /// Returns the exposure of a blast at `cx`, `cy`, `cz` through
    /// the sections of `rg`, with nothing read yet. `shapes` holds the
    /// collision boxes by block state, six doubles each.
    public static Exposure of(Region rg, Object[] shapes, double cx,
            double cy, double cz) {
        return new Exposure(rg, shapes, null, cx, cy, cz);
    }

    private Exposure(Region rg, Object[] shapes, Craters craters,
            double cx, double cy, double cz) {
        this.rg = rg;
        this.shapes = shapes;
        this.craters = craters;
        this.cx = cx;
        this.cy = cy;
        this.cz = cz;
        this.ox = (int) Math.floor(cx) - W / 2;
        this.oy = (int) Math.floor(cy) - W / 2;
        this.oz = (int) Math.floor(cz) - W / 2;
    }

    private int state(int x, int y, int z) {
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

    private boolean collides(int x, int y, int z) {
        return boxes(state(x, y, z)) != null;
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
        cells = new byte[W * W * W];
        for (int ix = 0; ix < W; ix++) {
            for (int iy = 0; iy < W; iy++) {
                for (int iz = 0; iz < W; iz++) {
                    cells[(ix * W + iy) * W + iz] =
                        (byte) (collides(ox + ix, oy + iy, oz + iz) ? 1 : 0);
                }
            }
        }
        sum();
    }

    private Exposure revise(Craters c) {
        if (cells == null) build();
        Exposure e = new Exposure(rg, shapes, c, cx, cy, cz);
        e.cells = cells.clone();
        int[] changed = c.inCube(ox, oy, oz, W);
        for (int i = 0; i < changed.length; i += 2) {
            e.cells[changed[i]] = (byte) (boxes(changed[i + 1]) == null ? 0 : 1);
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
            double fy, double fz, double tx, double ty, double tz) {
        double[] b = boxes(state(px, py, pz));
        if (b == null) return false;
        double dx = tx - fx, dy = ty - fy, dz = tz - fz;
        if (dx * dx + dy * dy + dz * dz < 1.0E-7) return false;
        double qx = fx + dx * 0.001, qy = fy + dy * 0.001;
        double qz = fz + dz * 0.001;
        if (inside(b, qx - px, qy - py, qz - pz)) return true;
        return clipBoxes(b, px, py, pz, fx, fy, fz, dx, dy, dz);
    }

    private boolean clip(double fx, double fy, double fz) {
        double tx = cx, ty = cy, tz = cz;
        if (Double.compare(fx, tx) == 0 && Double.compare(fy, ty) == 0
                && Double.compare(fz, tz) == 0) return false;
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
                        Math.max(bz, floor(toZ)) + 1) == 0) return false;
        if (shapeHit(bx, by, bz, fx, fy, fz, tx, ty, tz)) return true;
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
            if (shapeHit(bx, by, bz, fx, fy, fz, tx, ty, tz)) return true;
        }
        return false;
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
    /// `height` that see the blast of `e`. The cells in `c` that the
    /// earlier blasts of the tick changed count as they stand now.
    public static double density(Exposure e, Craters c, double px,
            double py, double pz, double half, double height) {
        double w = (float) half, h = (float) height;
        double x0 = px - w, y0 = py, z0 = pz - w;
        double x1 = px + w, y1 = py + h, z1 = pz + w;
        if (stale(e, c, px, py, pz, half, height)) {
            if (e.revised == null) e.revised = e.revise(c);
            return e.revised.seen(x0, y0, z0, x1, y1, z1);
        }
        return e.seen(x0, y0, z0, x1, y1, z1);
    }

    private double seen(double x0, double y0, double z0, double x1,
            double y1, double z1) {
        if (cells == null) build();
        if (collidingIn(floor(Math.min(cx, x0)) - 1,
                        floor(Math.min(cy, y0)) - 1,
                        floor(Math.min(cz, z0)) - 1,
                        floor(Math.max(cx, x1)) + 1,
                        floor(Math.max(cy, y1)) + 1,
                        floor(Math.max(cz, z1)) + 1) == 0) return 1.0;
        double xs = 1.0 / ((x1 - x0) * 2.0 + 1.0);
        double ys = 1.0 / ((y1 - y0) * 2.0 + 1.0);
        double zs = 1.0 / ((z1 - z0) * 2.0 + 1.0);
        double xo = (1.0 - Math.floor(1.0 / xs) * xs) / 2.0;
        double zo = (1.0 - Math.floor(1.0 / zs) * zs) / 2.0;
        int hits = 0, count = 0;
        for (double xx = 0.0; xx <= 1.0; xx += xs) {
            for (double yy = 0.0; yy <= 1.0; yy += ys) {
                for (double zz = 0.0; zz <= 1.0; zz += zs) {
                    double x = lerp(xx, x0, x1);
                    double y = lerp(yy, y0, y1);
                    double z = lerp(zz, z0, z1);
                    if (!clip(x + xo, y, z + zo)) hits++;
                    count++;
                }
            }
        }
        return (double) ((float) hits / (float) count);
    }
}
