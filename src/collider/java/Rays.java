package collider.java;

/// Line of sight through a rectangular grid of sections.
public final class Rays {

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
}
