package collider.world.space;

/// The cells around a blast that block its sight, to share among the
/// bodies it reaches.
public final class Exposure {

    /// The width of the cube of cells that an exposure keeps.
    public static final int W = 24;

    private static final int S = W + 1;

    private final Region rg;
    private final boolean[] solid;
    private final int ox, oy, oz;
    private byte[] cells;
    private int[] sums;

    /// Returns the exposure of a blast at `cx`, `cy`, `cz` through
    /// the sections of `rg`, with nothing read yet.
    public static Exposure of(Region rg, boolean[] solid, double cx,
            double cy, double cz) {
        return new Exposure(rg, solid, cx, cy, cz);
    }

    /// Makes the exposure of a blast at `cx`, `cy`, `cz` through the
    /// sections of `rg`. A block blocks sight when its state is `true`
    /// in `solid`.
    public Exposure(Region rg, boolean[] solid, double cx, double cy,
            double cz) {
        this.rg = rg;
        this.solid = solid;
        this.ox = (int) Math.floor(cx) - W / 2;
        this.oy = (int) Math.floor(cy) - W / 2;
        this.oz = (int) Math.floor(cz) - W / 2;
    }

    private boolean solidCell(int x, int y, int z) {
        int st = Rays.readBlock(rg.grid(), rg.cx0(), rg.cz0(), rg.sy0(),
                                rg.ncx(), rg.ncz(), rg.nsy(), x, y, z);
        return st < solid.length && solid[st];
    }

    private static int sumIndex(int ix, int iy, int iz) {
        return (ix * S + iy) * S + iz;
    }

    private void build() {
        cells = new byte[W * W * W];
        sums = new int[S * S * S];
        for (int ix = 0; ix < W; ix++) {
            for (int iy = 0; iy < W; iy++) {
                for (int iz = 0; iz < W; iz++) {
                    int c = solidCell(ox + ix, oy + iy, oz + iz) ? 1 : 0;
                    cells[(ix * W + iy) * W + iz] = (byte) c;
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

    private boolean inside(int ix, int iy, int iz) {
        return ix >= 0 && ix < W && iy >= 0 && iy < W && iz >= 0
            && iz < W;
    }

    private boolean solidAt(int x, int y, int z) {
        int ix = x - ox, iy = y - oy, iz = z - oz;
        if (!inside(ix, iy, iz)) return solidCell(x, y, z);
        return cells[(ix * W + iy) * W + iz] != 0;
    }

    private int solidIn(int x0, int y0, int z0, int x1, int y1, int z1) {
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

    private static int low(double a, double b) {
        return (int) Math.floor(Math.min(a, b)) - 1;
    }

    private static int high(double a, double b) {
        return (int) Math.floor(Math.max(a, b)) + 1;
    }

    private static int cell(double c, double s, long i) {
        return (int) Math.floor(c + s * (i * 0.3));
    }

    private boolean solidOn(double cx, double cy, double cz, double sx,
            double sy, double sz, long ia, long ib) {
        int ax = cell(cx, sx, ia), ay = cell(cy, sy, ia);
        int az = cell(cz, sz, ia);
        int bx = cell(cx, sx, ib), by = cell(cy, sy, ib);
        int bz = cell(cz, sz, ib);
        int n = solidIn(Math.min(ax, bx), Math.min(ay, by),
                        Math.min(az, bz), Math.max(ax, bx),
                        Math.max(ay, by), Math.max(az, bz));
        if (n == 0) return false;
        if (n < 0 || ib - ia < 4) {
            for (long i = ia; i <= ib; i++) {
                if (solidAt(cell(cx, sx, i), cell(cy, sy, i),
                            cell(cz, sz, i))) return true;
            }
            return false;
        }
        long mid = (ia + ib) >>> 1;
        return solidOn(cx, cy, cz, sx, sy, sz, ia, mid)
            || solidOn(cx, cy, cz, sx, sy, sz, mid + 1, ib);
    }

    private long clear(double cx, double cy, double cz, double px,
            double py, double pz) {
        double dx = px - cx, dy = py - cy, dz = pz - cz;
        double len = Math.sqrt(dx * dx + dy * dy + dz * dz);
        if (len < 0.3) return 1;
        long steps = (long) (len / 0.3);
        double sx = dx / len, sy = dy / len, sz = dz / len;
        return solidOn(cx, cy, cz, sx, sy, sz, 1, steps) ? 0 : 1;
    }

    /// Returns the share from 0.0 to 1.0 of the points of a body box
    /// at `px`, `py`, `pz` with half width `half` and height `height`
    /// that see the blast of `e` at `cx`, `cy`, `cz`.
    public static double density(Exposure e, double cx, double cy,
            double cz, double px, double py, double pz, double half,
            double height) {
        return e.seen(cx, cy, cz, px, py, pz, half, height);
    }

    private double seen(double cx, double cy, double cz, double px,
            double py, double pz, double half, double height) {
        if (cells == null) build();
        if (solidIn(low(cx, px - half), low(cy, py), low(cz, pz - half),
                    high(cx, px + half), high(cy, py + height),
                    high(cz, pz + half)) == 0) return 1.0;
        double sx = 1.0 / (4.0 * half + 1.0);
        double sy = 1.0 / (2.0 * height + 1.0);
        double ox = (1.0 - Math.floor(1.0 / sx) * sx) / 2.0;
        long hit = 0, total = 0;
        for (double fx = 0.0; fx <= 1.0; fx += sx) {
            for (double fy = 0.0; fy <= 1.0; fy += sy) {
                for (double fz = 0.0; fz <= 1.0; fz += sx) {
                    double x = px - half + fx * 2.0 * half + ox;
                    double y = py + fy * height;
                    double z = pz - half + fz * 2.0 * half + ox;
                    hit += clear(cx, cy, cz, x, y, z);
                    total++;
                }
            }
        }
        return total == 0 ? 0.0 : (double) hit / (double) total;
    }
}
