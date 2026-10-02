package collider.world.space;

import clojure.lang.IFn;
import collider.world.Collision;

/// The cells around a blast that may block its sight, to share among
/// the bodies it reaches. Sight follows `ServerExplosion.getSeenPercent`:
/// a clip from each sample point of the body to the blast against the
/// collision boxes of the blocks.
public final class Exposure {

    /// The width of the cube of cells that an exposure keeps.
    public static final int W = 24;

    private static final int S = W + 1;

    private final Region rg;
    private final Object[] shapes;
    private final byte[] kinds;
    private final double cx, cy, cz;
    private final int ox, oy, oz;
    private char[] states;
    private char[] sums;
    private byte whole;

    /// Returns the exposure of a blast at `cx`, `cy`, `cz` through
    /// the sections of `rg`, with nothing read yet. `shapes` holds the
    /// collision boxes by block state, six doubles each, as an empty
    /// context meets them at the origin. `kinds` holds the kind of
    /// each block state.
    public static Exposure of(Region rg, Object[] shapes, byte[] kinds, double cx, double cy, double cz) {
        return new Exposure(rg, shapes, kinds, cx, cy, cz);
    }

    private Exposure(Region rg, Object[] shapes, byte[] kinds, double cx, double cy, double cz) {
        this.rg = rg;
        this.shapes = shapes;
        this.kinds = kinds;
        this.cx = cx;
        this.cy = cy;
        this.cz = cz;
        this.ox = (int) Math.floor(cx) - W / 2;
        this.oy = (int) Math.floor(cy) - W / 2;
        this.oz = (int) Math.floor(cz) - W / 2;
    }

    private int state(int x, int y, int z) {
        int ix = x - ox, iy = y - oy, iz = z - oz;
        if (states != null && ix >= 0 && ix < W && iy >= 0 && iy < W && iz >= 0 && iz < W) {
            return states[(ix * W + iy) * W + iz];
        }
        return read(x, y, z);
    }

    private int read(int x, int y, int z) {
        return Rays.readBlock(rg.grid(), rg.cx0(), rg.cz0(), rg.sy0(), rg.ncx(), rg.ncz(), rg.nsy(), x, y, z);
    }

    private boolean whole() {
        if (whole == 0) whole = (byte) (present() ? 1 : -1);
        return whole > 0;
    }

    private boolean present() {
        if (rg.readAbsent() == null) return true;
        int x0 = Math.max((ox >> 4) - rg.cx0(), 0);
        int x1 = Math.min(((ox + W - 1) >> 4) - rg.cx0(), rg.ncx() - 1);
        int z0 = Math.max((oz >> 4) - rg.cz0(), 0);
        int z1 = Math.min(((oz + W - 1) >> 4) - rg.cz0(), rg.ncz() - 1);
        for (int ix = x0; ix <= x1; ix++) {
            for (int iz = z0; iz <= z1; iz++) {
                if (rg.cols()[ix * rg.ncz() + iz] == null) return false;
            }
        }
        return true;
    }

    /// Returns the block state at `x`, `y`, `z` in the region of `e`
    /// as `Rays.block` reads it, from the cells of `e` when no column
    /// under them waits to be read.
    public static int block(Exposure e, IFn summon, int x, int y, int z) {
        int ix = x - e.ox, iy = y - e.oy, iz = z - e.oz;
        if (ix >= 0 && ix < W && iy >= 0 && iy < W && iz >= 0 && iz < W && e.whole()) {
            if (e.sums == null) e.build();
            return e.states[(ix * W + iy) * W + iz];
        }
        return Rays.block(e.rg, summon, x, y, z);
    }

    private boolean mayCollide(int st) {
        return Collision.mayCollide(shapes, kinds, st);
    }

    private double[] shape(int st, int x, int y, int z, double bottom, int flags) {
        return Collision.shape(shapes, kinds, st, x, y, z, bottom, flags);
    }

    private static int sumIndex(int ix, int iy, int iz) {
        return (ix * S + iy) * S + iz;
    }

    private void build() {
        char[] st = new char[W * W * W];
        char[] sm = new char[S * S * S];
        for (int ix = 0; ix < W; ix++) {
            for (int iy = 0; iy < W; iy++) {
                for (int iz = 0; iz < W; iz++) {
                    int b = read(ox + ix, oy + iy, oz + iz);
                    st[(ix * W + iy) * W + iz] = (char) b;
                    sm[sumIndex(ix + 1, iy + 1, iz + 1)] = (char) ((mayCollide(b) ? 1 : 0)
                            + sm[sumIndex(ix, iy + 1, iz + 1)]
                            + sm[sumIndex(ix + 1, iy, iz + 1)]
                            + sm[sumIndex(ix + 1, iy + 1, iz)]
                            - sm[sumIndex(ix, iy, iz + 1)]
                            - sm[sumIndex(ix, iy + 1, iz)]
                            - sm[sumIndex(ix + 1, iy, iz)]
                            + sm[sumIndex(ix, iy, iz)]);
                }
            }
        }
        states = st;
        sums = sm;
    }

    private int collidingIn(int x0, int y0, int z0, int x1, int y1, int z1) {
        int ax = x0 - ox, ay = y0 - oy, az = z0 - oz;
        int bx = x1 - ox + 1, by = y1 - oy + 1, bz = z1 - oz + 1;
        if (ax < 0 || ay < 0 || az < 0 || bx > W || by > W || bz > W) {
            return -1;
        }
        return sums[sumIndex(bx, by, bz)]
                - sums[sumIndex(ax, by, bz)]
                - sums[sumIndex(bx, ay, bz)]
                - sums[sumIndex(bx, by, az)]
                + sums[sumIndex(ax, ay, bz)]
                + sums[sumIndex(ax, by, az)]
                + sums[sumIndex(bx, ay, az)]
                - sums[sumIndex(ax, ay, az)];
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

    @SuppressWarnings("UseCompareMethod")
    private static int sign(double v) {
        return v == 0.0 ? 0 : (v > 0.0 ? 1 : -1);
    }

    private static boolean inside(double[] b, double x, double y, double z) {
        for (int k = 0; k < b.length; k += 6) {
            if (x >= b[k] && x < b[k + 3] && y >= b[k + 1] && y < b[k + 4] && z >= b[k + 2] && z < b[k + 5]) {
                return true;
            }
        }
        return false;
    }

    private static double clipPoint(
            double scale,
            double da,
            double db,
            double dc,
            double point,
            double minB,
            double maxB,
            double minC,
            double maxC,
            double fromA,
            double fromB,
            double fromC) {
        double s = (point - fromA) / da;
        double pb = fromB + s * db;
        double pc = fromC + s * dc;
        if (0.0 < s
                && s < scale
                && minB - 1.0E-7 < pb
                && pb < maxB + 1.0E-7
                && minC - 1.0E-7 < pc
                && pc < maxC + 1.0E-7) return s;
        return -1.0;
    }

    private static boolean clipBoxes(
            double[] b, int px, int py, int pz, double fx, double fy, double fz, double dx, double dy, double dz) {
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

    private boolean shapeHit(
            int px,
            int py,
            int pz,
            double fx,
            double fy,
            double fz,
            double tx,
            double ty,
            double tz,
            double bottom,
            int flags) {
        double[] b = shape(state(px, py, pz), px, py, pz, bottom, flags);
        if (b == null) return false;
        double dx = tx - fx, dy = ty - fy, dz = tz - fz;
        if (dx * dx + dy * dy + dz * dz < 1.0E-7) return false;
        double qx = fx + dx * 0.001, qy = fy + dy * 0.001;
        double qz = fz + dz * 0.001;
        if (inside(b, qx - px, qy - py, qz - pz)) return true;
        return clipBoxes(b, px, py, pz, fx, fy, fz, dx, dy, dz);
    }

    private boolean clip(double fx, double fy, double fz, double bottom, int flags) {
        double tx = cx, ty = cy, tz = cz;
        if (Double.compare(fx, tx) == 0 && Double.compare(fy, ty) == 0 && Double.compare(fz, tz) == 0) return false;
        double toX = lerp(-1.0E-7, tx, fx), toY = lerp(-1.0E-7, ty, fy);
        double toZ = lerp(-1.0E-7, tz, fz);
        double frX = lerp(-1.0E-7, fx, tx), frY = lerp(-1.0E-7, fy, ty);
        double frZ = lerp(-1.0E-7, fz, tz);
        int bx = floor(frX), by = floor(frY), bz = floor(frZ);
        if (collidingIn(
                        Math.min(bx, floor(toX)) - 1,
                        Math.min(by, floor(toY)) - 1,
                        Math.min(bz, floor(toZ)) - 1,
                        Math.max(bx, floor(toX)) + 1,
                        Math.max(by, floor(toY)) + 1,
                        Math.max(bz, floor(toZ)) + 1)
                == 0) return false;
        if (shapeHit(bx, by, bz, fx, fy, fz, tx, ty, tz, bottom, flags)) {
            return true;
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
                return true;
            }
        }
        return false;
    }

    /// Returns the share from 0.0 to 1.0 of the sample points of a body
    /// box at `px`, `py`, `pz` with half width `half` and height
    /// `height` that see the blast of `e`. `flags` tell how the body
    /// meets the blocks whose shape depends on it.
    public static double density(Exposure e, double px, double py, double pz, double half, double height, int flags) {
        return e.sight(px, py, pz, half, height, flags);
    }

    private double sight(double px, double py, double pz, double half, double height, int flags) {
        if (sums == null) build();
        double w = (float) half, h = (float) height;
        double x0 = px - w, z0 = pz - w;
        double x1 = px + w, y1 = py + h, z1 = pz + w;
        double xs = 1.0 / ((x1 - x0) * 2.0 + 1.0);
        double ys = 1.0 / ((y1 - py) * 2.0 + 1.0);
        double zs = 1.0 / ((z1 - z0) * 2.0 + 1.0);
        double xo = (1.0 - Math.floor(1.0 / xs) * xs) / 2.0;
        double zo = (1.0 - Math.floor(1.0 / zs) * zs) / 2.0;
        boolean clear = collidingIn(
                        floor(Math.min(cx, x0)) - 1,
                        floor(Math.min(cy, py)) - 1,
                        floor(Math.min(cz, z0)) - 1,
                        floor(Math.max(cx, x1)) + 1,
                        floor(Math.max(cy, y1)) + 1,
                        floor(Math.max(cz, z1)) + 1)
                == 0;
        int hits = 0, count = 0;
        for (double xx = 0.0; xx <= 1.0; xx += xs) {
            for (double yy = 0.0; yy <= 1.0; yy += ys) {
                for (double zz = 0.0; zz <= 1.0; zz += zs) {
                    double x = lerp(xx, x0, x1) + xo;
                    double y = lerp(yy, py, y1);
                    double z = lerp(zz, z0, z1) + zo;
                    if (clear || !clip(x, y, z, py, flags)) hits++;
                    count++;
                }
            }
        }
        return (float) hits / (float) count;
    }
}
