package collider.java;

import clojure.lang.IFn;

/// Line of sight through a rectangular grid of sections, and the
/// rays of a blast.
public final class Rays {

    private static final int MIN_Y = -64;

    private static final int MAX_Y = 319;

    /// The width of the cube of cells that the rays of a blast mark.
    public static final int W = 21;

    /// Returns the block state at `x`, `y`, `z` in a `grid` of
    /// sections shaped `ncx` by `ncz` by `nsy`, whose first cell
    /// covers section coordinates `cx0`, `cz0`, `sy0`, or 0 if
    /// outside the grid.
    public static int readBlock(Object[] grid, int cx0, int cz0, int sy0,
                                int ncx, int ncz, int nsy,
                                int x, int y, int z) {
        int ix = (x >> 4) - cx0;
        int iz = (z >> 4) - cz0;
        int iy = (y >> 4) - sy0;
        if (ix < 0 || ix >= ncx || iz < 0 || iz >= ncz || iy < 0 || iy >= nsy) return 0;
        Section s = (Section) grid[(ix * ncz + iz) * nsy + iy];
        if (s == null) return 0;
        return s.block(((y & 15) << 8) | ((z & 15) << 4) | (x & 15));
    }
    /// Returns 1 if the straight path from `cx`, `cy`, `cz` to
    /// `px`, `py`, `pz` crosses no block whose state is `true` in
    /// `solid`, otherwise 0.
    public static long clearPath(Object[] grid, int cx0, int cz0, int sy0,
                                 int ncx, int ncz, int nsy, boolean[] solid,
                                 double cx, double cy, double cz,
                                 double px, double py, double pz) {
        double dx = px - cx, dy = py - cy, dz = pz - cz;
        double len = Math.sqrt(dx * dx + dy * dy + dz * dz);
        if (len < 0.3) return 1;
        long steps = (long) (len / 0.3);
        double sx = dx / len, sy = dy / len, sz = dz / len;
        for (long i = 1; i <= steps; i++) {
            double t = i * 0.3;
            int st = readBlock(grid, cx0, cz0, sy0, ncx, ncz, nsy,
                               (int) Math.floor(cx + sx * t),
                               (int) Math.floor(cy + sy * t),
                               (int) Math.floor(cz + sz * t));
            if (st < solid.length && solid[st]) return 0;
        }
        return 1;
    }

    /// Returns the share, 0.0 to 1.0, of the points of a body box
    /// at `px`, `py`, `pz` with half width `half` and height
    /// `height` that `clearPath` joins to `cx`, `cy`, `cz` through
    /// the sections of `rg`.
    public static double density(Region rg, boolean[] solid,
            double cx, double cy, double cz, double px, double py,
            double pz, double half, double height) {
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
                    hit += clearPath(rg.grid(), rg.cx0(), rg.cz0(),
                                     rg.sy0(), rg.ncx(), rg.ncz(),
                                     rg.nsy(), solid, cx, cy, cz,
                                     x, y, z);
                    total++;
                }
            }
        }
        return total == 0 ? 0.0 : (double) hit / (double) total;
    }

    /// Returns the block state at `x`, `y`, `z` in `rg`, or 0
    /// outside it. An absent column of a region that reads absent
    /// chunks is first handed to `summon` as its grid x and z.
    public static int block(Region rg, IFn summon, int x, int y,
            int z) {
        int ix = (x >> 4) - rg.cx0();
        int iz = (z >> 4) - rg.cz0();
        int iy = (y >> 4) - rg.sy0();
        if (ix < 0 || ix >= rg.ncx() || iz < 0 || iz >= rg.ncz()
                || iy < 0 || iy >= rg.nsy()) return 0;
        int col = ix * rg.ncz() + iz;
        if (rg.cols()[col] == null && rg.readAbsent() != null) {
            summon.invoke((long) ix, (long) iz);
        }
        Section s = (Section) rg.grid()[col * rg.nsy() + iy];
        if (s == null) return 0;
        return s.block(((y & 15) << 8) | ((z & 15) << 4) | (x & 15));
    }

    private static long mix64(long z) {
        z = (z ^ (z >>> 30)) * -4658895280553007687L;
        z = (z ^ (z >>> 27)) * -7723592293110705685L;
        return z ^ (z >>> 31);
    }

    private static double unit(long a, long b, long c) {
        long h = mix64(mix64(mix64(a) + b) + c);
        return (double) (h & 0xFFFFFF) / 1.6777216E7;
    }

    private static int cellIndex(long ix, long iy, long iz) {
        if (ix < 0 || ix >= W || iy < 0 || iy >= W || iz < 0
                || iz >= W) return -1;
        return (int) ((ix * W + iy) * W + iz);
    }

    private static void castRay(Region rg, IFn summon, double[] resist,
            byte[] hit, long ox, long oy, long oz, double x, double y,
            double z, double d0, double d1, double d2, double f) {
        int prev = -1, st = 0;
        while (true) {
            long by = (long) Math.floor(y);
            if (!(f > 0.0 && MIN_Y <= by && by <= MAX_Y)) return;
            long bx = (long) Math.floor(x);
            long bz = (long) Math.floor(z);
            int i = cellIndex(bx - ox, by - oy, bz - oz);
            boolean same = i >= 0 && i == prev;
            if (!same) st = block(rg, summon, (int) bx, (int) by,
                                  (int) bz);
            if (st != 0) {
                double r = st < resist.length ? resist[st] : 3.0;
                f = f - (r + 0.3) * 0.3;
            }
            if (f > 0.0 && !same && i >= 0) {
                hit[i] = (byte) (st == 0 ? 1 : 2);
            }
            f = f - 0.22500001;
            x = x + d0 * 0.3;
            y = y + d1 * 0.3;
            z = z + d2 * 0.3;
            prev = i;
        }
    }

    /// Casts the rays of a blast of `power` at `cx`, `cy`, `cz`
    /// through `rg` and marks in `hit` each cell of the `W` cube
    /// at `ox`, `oy`, `oz` that a ray reaches: 1 for air, 2 for a
    /// block. `seed` varies the power of each ray; `resist` holds
    /// the blast resistance by block state.
    public static void cast(Region rg, IFn summon, double[] resist,
            byte[] hit, long ox, long oy, long oz, double cx,
            double cy, double cz, double power, long seed) {
        for (long j = 0; j < 16; j++) {
            for (long k = 0; k < 16; k++) {
                for (long l = 0; l < 16; l++) {
                    if (j == 0 || j == 15 || k == 0 || k == 15
                            || l == 0 || l == 15) {
                        double d0 = j / 7.5 - 1.0;
                        double d1 = k / 7.5 - 1.0;
                        double d2 = l / 7.5 - 1.0;
                        double d3 = Math.sqrt(d0 * d0 + d1 * d1
                                              + d2 * d2);
                        double f = power * (0.7 + 0.6
                                * unit(seed, j, 31 * k + l));
                        castRay(rg, summon, resist, hit, ox, oy, oz,
                                cx, cy, cz, d0 / d3, d1 / d3, d2 / d3,
                                f);
                    }
                }
            }
        }
    }

    /// Returns the number of cells in `hit` that a ray reached.
    public static long hitCount(byte[] hit) {
        long n = 0;
        for (byte b : hit) {
            if (b != 0) n++;
        }
        return n;
    }
}
