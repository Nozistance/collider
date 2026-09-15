package collider.java;

/** The perlin noise of 26.2 worldgen, built from a seed and its octave amplitudes. */
public final class Noise {
    private static final int[][] GRADIENT = {
            {1, 1, 0}, {-1, 1, 0}, {1, -1, 0}, {-1, -1, 0},
            {1, 0, 1}, {-1, 0, 1}, {1, 0, -1}, {-1, 0, -1},
            {0, 1, 1}, {0, -1, 1}, {0, 1, -1}, {0, -1, -1},
            {1, 1, 0}, {0, -1, 1}, {-1, 1, 0}, {0, -1, -1}};

    private static final double INPUT_FACTOR = 1.0181268882175227;

    private static final class Bits {
        private long seed;

        Bits(long seed) {
            this.seed = (seed ^ 25214903917L) & 281474976710655L;
        }

        int next(int bits) {
            seed = (seed * 25214903917L + 11L) & 281474976710655L;
            return (int) (seed >> (48 - bits));
        }

        int nextInt(int bound) {
            if ((bound & (bound - 1)) == 0) {
                return (int) (((long) bound * (long) next(31)) >> 31);
            }
            int sample;
            int modulo;
            do {
                sample = next(31);
                modulo = sample % bound;
            } while (sample - modulo + (bound - 1) < 0);
            return modulo;
        }

        long nextLong() {
            return ((long) next(32) << 32) + (long) next(32);
        }

        double nextDouble() {
            return (double) (((long) next(26) << 27) + (long) next(27)) * 1.110223E-16F;
        }
    }

    private static final class Octave {
        private final byte[] p = new byte[256];
        private final double xo;
        private final double yo;
        private final double zo;

        Octave(Bits random) {
            this.xo = random.nextDouble() * 256.0;
            this.yo = random.nextDouble() * 256.0;
            this.zo = random.nextDouble() * 256.0;
            for (int i = 0; i < 256; i++) {
                p[i] = (byte) i;
            }
            for (int i = 0; i < 256; i++) {
                int offset = random.nextInt(256 - i);
                byte tmp = p[i];
                p[i] = p[i + offset];
                p[i + offset] = tmp;
            }
        }

        private int p(int x) {
            return p[x & 0xFF] & 0xFF;
        }

        private static double grad(int hash, double x, double y, double z) {
            int[] g = GRADIENT[hash & 15];
            return g[0] * x + g[1] * y + g[2] * z;
        }

        private static double smooth(double t) {
            return t * t * t * (t * (t * 6.0 - 15.0) + 10.0);
        }

        private static double lerp(double t, double a, double b) {
            return a + t * (b - a);
        }

        double noise(double px, double py, double pz) {
            double x = px + xo;
            double y = py + yo;
            double z = pz + zo;
            int xf = (int) Math.floor(x);
            int yf = (int) Math.floor(y);
            int zf = (int) Math.floor(z);
            double xr = x - xf;
            double yr = y - yf;
            double zr = z - zf;
            return sample(xf, yf, zf, xr, yr, zr);
        }

        private double sample(int x, int y, int z, double xr, double yr, double zr) {
            int x0 = p(x);
            int x1 = p(x + 1);
            int xy00 = p(x0 + y);
            int xy01 = p(x0 + y + 1);
            int xy10 = p(x1 + y);
            int xy11 = p(x1 + y + 1);
            double d000 = grad(p(xy00 + z), xr, yr, zr);
            double d100 = grad(p(xy10 + z), xr - 1.0, yr, zr);
            double d010 = grad(p(xy01 + z), xr, yr - 1.0, zr);
            double d110 = grad(p(xy11 + z), xr - 1.0, yr - 1.0, zr);
            double d001 = grad(p(xy00 + z + 1), xr, yr, zr - 1.0);
            double d101 = grad(p(xy10 + z + 1), xr - 1.0, yr, zr - 1.0);
            double d011 = grad(p(xy01 + z + 1), xr, yr - 1.0, zr - 1.0);
            double d111 = grad(p(xy11 + z + 1), xr - 1.0, yr - 1.0, zr - 1.0);
            double xa = smooth(xr);
            double ya = smooth(yr);
            double za = smooth(zr);
            return lerp(za,
                    lerp(ya, lerp(xa, d000, d100), lerp(xa, d010, d110)),
                    lerp(ya, lerp(xa, d001, d101), lerp(xa, d011, d111)));
        }
    }

    private static final class Perlin {
        private final Octave[] levels;
        private final double[] amplitudes;
        private final double inputFactor;
        private final double valueFactor;

        Perlin(Bits random, int firstOctave, double[] amplitudes) {
            this.amplitudes = amplitudes;
            int octaves = amplitudes.length;
            int zeroIndex = -firstOctave;
            this.levels = new Octave[octaves];
            long fork = random.nextLong();
            for (int i = 0; i < octaves; i++) {
                if (amplitudes[i] != 0.0) {
                    String name = "octave_" + (firstOctave + i);
                    levels[i] = new Octave(new Bits(name.hashCode() ^ fork));
                }
            }
            this.inputFactor = Math.pow(2.0, -zeroIndex);
            this.valueFactor = Math.pow(2.0, octaves - 1) / (Math.pow(2.0, octaves) - 1.0);
        }

        private static double wrap(double x) {
            return x - (double) (long) Math.floor(x / 3.3554432E7 + 0.5) * 3.3554432E7;
        }

        double value(double x, double y, double z) {
            double value = 0.0;
            double factor = inputFactor;
            double scale = valueFactor;
            for (int i = 0; i < levels.length; i++) {
                if (levels[i] != null) {
                    value += amplitudes[i] * scale
                            * levels[i].noise(wrap(x * factor), wrap(y * factor), wrap(z * factor));
                }
                factor *= 2.0;
                scale /= 2.0;
            }
            return value;
        }
    }

    private final Perlin first;
    private final Perlin second;
    private final double valueFactor;

    private Noise(long seed, int firstOctave, double[] amplitudes) {
        Bits random = new Bits(seed);
        this.first = new Perlin(random, firstOctave, amplitudes);
        this.second = new Perlin(random, firstOctave, amplitudes);
        int lowest = Integer.MAX_VALUE;
        int highest = Integer.MIN_VALUE;
        for (int i = 0; i < amplitudes.length; i++) {
            if (amplitudes[i] != 0.0) {
                lowest = Math.min(lowest, i);
                highest = Math.max(highest, i);
            }
        }
        this.valueFactor = 0.16666666666666666 / (0.1 * (1.0 + 1.0 / (double) (highest - lowest + 1)));
    }

    /** Returns the noise of a seed, its first octave and its octave amplitudes. */
    public static Noise of(long seed, int firstOctave, double[] amplitudes) {
        return new Noise(seed, firstOctave, amplitudes);
    }

    /** Returns the noise at a point scaled by a float factor. */
    public double at(double x, double y, double z, float scale) {
        return value(x * (double) scale, y * (double) scale, z * (double) scale);
    }

    /** Returns the noise at a block position scaled in float arithmetic. */
    public double atFloat(int x, int y, int z, float scale) {
        return value((float) x * scale, (float) y * scale, (float) z * scale);
    }

    /** Returns the value of the noise at a point, from -1 to 1. */
    public double value(double x, double y, double z) {
        return (first.value(x, y, z)
                + second.value(x * INPUT_FACTOR, y * INPUT_FACTOR, z * INPUT_FACTOR)) * valueFactor;
    }
}
