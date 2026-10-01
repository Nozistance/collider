package collider.world.space;

import collider.RandomSupport;
import collider.world.Section;
import clojure.lang.IFn;

/// The rays of a blast through a grid of sections.
public final class Rays {

    private static final int MIN_Y = -64;

    private static final int MAX_Y = 319;

    /// The width of the cube of cells that the rays of a blast mark.
    public static final int W = 21;

    /// Returns the block state at `x`, `y`, `z` in a `grid` of
    /// sections shaped `ncx` by `ncz` by `nsy`, or 0 outside
    /// the grid. The first cell covers the section at `cx0`,
    /// `cz0`, `sy0`.
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

    /// Returns the block state at `x`, `y`, `z` in `rg`, or 0 outside
    /// it. An absent column of a region that reads absent chunks goes
    /// to `summon` as its grid x and z.
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

    private static int cellIndex(long ix, long iy, long iz) {
        if (ix < 0 || ix >= W || iy < 0 || iy >= W || iz < 0
                || iz >= W) return -1;
        return (int) ((ix * W + iy) * W + iz);
    }

    private static final double STEP = 0.3F;

    private final Exposure seen;
    private final IFn summon;
    private final float[] resist;
    private final long ox, oy, oz;
    private final double cx, cy, cz;
    private final float power;
    private final long seed;
    private final byte[] vals = new byte[W * W * W];

    private Rays(Exposure seen, IFn summon, float[] resist, long ox,
            long oy, long oz, double cx, double cy, double cz,
            float power, long seed) {
        this.seen = seen;
        this.summon = summon;
        this.resist = resist;
        this.ox = ox;
        this.oy = oy;
        this.oz = oz;
        this.cx = cx;
        this.cy = cy;
        this.cz = cz;
        this.power = power;
        this.seed = seed;
    }

    private static double axis(long j) {
        return (double) ((float) j / 15.0F * 2.0F - 1.0F);
    }

    private byte val(int st) {
        return (byte) (Float.isNaN(resist[st]) ? 1 : 2);
    }

    private void castRay(long j, long k, long l) {
        double xd = axis(j), yd = axis(k), zd = axis(l);
        double d = Math.sqrt(xd * xd + yd * yd + zd * zd);
        xd /= d;
        yd /= d;
        zd /= d;
        float f = power * (0.7F
                + (float) RandomSupport.unit(seed, j, 31 * k + l) * 0.6F);
        double x = cx, y = cy, z = cz;
        int prev = -1, st = 0;
        boolean first = true;
        while (f > 0.0F) {
            int bx = (int) Math.floor(x), by = (int) Math.floor(y);
            int bz = (int) Math.floor(z);
            int i = cellIndex(bx - ox, by - oy, bz - oz);
            boolean same = !first && i >= 0 && i == prev;
            first = false;
            if (by < MIN_Y || by > MAX_Y) break;
            if (!same) st = Exposure.block(seen, summon, bx, by, bz);
            float res = st < resist.length ? resist[st] : 3.0F;
            if (!Float.isNaN(res)) f -= (res + 0.3F) * 0.3F;
            if (f > 0.0F && !same && i >= 0) vals[i] = val(st);
            x += xd * STEP;
            y += yd * STEP;
            z += zd * STEP;
            prev = i;
            f -= 0.22500001F;
        }
    }

    private static boolean surface(long j, long k, long l) {
        return j == 0 || j == 15 || k == 0 || k == 15 || l == 0
            || l == 15;
    }

    /// Casts the rays of a blast of `power` at `cx`, `cy`, `cz`
    /// through the cells that `seen` reads. The rays mark the cells
    /// of the `W` cube at `ox`, `oy`, `oz` they reach. `seed` varies
    /// the power of each ray. `resist` holds the blast resistance by
    /// block state, NaN for air.
    public static Rays cast(Exposure seen, IFn summon, float[] resist,
            long ox, long oy, long oz, double cx, double cy, double cz,
            double power, long seed) {
        Rays rs = new Rays(seen, summon, resist, ox, oy, oz, cx, cy, cz,
                           (float) power, seed);
        for (long j = 0; j < 16; j++) {
            for (long k = 0; k < 16; k++) {
                for (long l = 0; l < 16; l++) {
                    if (surface(j, k, l)) rs.castRay(j, k, l);
                }
            }
        }
        return rs;
    }

    /// Returns the cells the rays of `rs` reach: 1 for air and 2 for
    /// a block, by cell of the `W` cube.
    public static byte[] hit(Rays rs) {
        return rs.vals.clone();
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
