package collider.java;

public final class Rays {

    public static int readBlock(Object[] grid, int cx0, int cz0, int sy0,
                                int ncx, int ncz, int nsy,
                                int x, int y, int z) {
        int ix = (x >> 4) - cx0;
        int iz = (z >> 4) - cz0;
        int iy = (y >> 4) - sy0;
        if (ix < 0 || ix >= ncx || iz < 0 || iz >= ncz || iy < 0 || iy >= nsy) return 0;
        short[] blocks = (short[]) grid[(ix * ncz + iz) * nsy + iy];
        if (blocks == null) return 0;
        return blocks[((y & 15) << 8) | ((z & 15) << 4) | (x & 15)] & 0xFFFF;
    }
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
            int id = st >> 4;
            if (id < 256 && solid[id]) return 0;
        }
        return 1;
    }
}
