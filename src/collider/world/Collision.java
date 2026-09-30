package collider.world;

/// The collision boxes of a block state as a body meets them, as
/// `BlockState.getCollisionShape` with the `CollisionContext` of the
/// body: scaffolding and powder snow depend on the body, bamboo and
/// the speleothems on the position of the block.
public final class Collision {

    private Collision() {}

    /// The box of a full cube.
    public static final double[] CUBE = {0, 0, 0, 1, 1, 1};

    private static final double[] UNSTABLE_BOTTOM = {0, 0, 0, 1, 0.125, 1};

    private static final double[] SNOW_FALLING = {0, 0, 0, 1, 0.9F, 1};

    /// The kind of a block state whose collision shape depends on the
    /// body that meets it or on the position of the block.
    public static final byte PLAIN = 0, SCAFFOLDING = 1,
        SCAFFOLDING_HANGING = 2, POWDER_SNOW = 3, OFFSET_QUARTER = 4,
        OFFSET_EIGHTH = 5;

    /// The flags of a body: it descends, it falls more than 2.5
    /// blocks, it walks on powder snow, it is a falling block.
    public static final int DESCENDING = 1, FALLING = 2, WALKER = 4,
        FALLING_BLOCK = 8;

    /// Returns the boxes of `st` at the origin as an empty context
    /// meets them, or null when it has none. A state past the end of
    /// `shapes` counts as a full cube.
    public static double[] boxes(Object[] shapes, int st) {
        if (st <= 0) return null;
        if (st >= shapes.length) return CUBE;
        double[] b = (double[]) shapes[st];
        return b.length == 0 ? null : b;
    }

    /// Returns the kind of `st` in `kinds`.
    public static int kind(byte[] kinds, int st) {
        return st > 0 && st < kinds.length ? kinds[st] : PLAIN;
    }

    /// Returns true when some body meets boxes of `st`.
    public static boolean mayCollide(Object[] shapes, byte[] kinds,
            int st) {
        return boxes(shapes, st) != null || kind(kinds, st) == POWDER_SNOW;
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

    /// Returns the y coordinates of `shape`, the boxes that `shape`
    /// or `boxes` gave for `st`, in `ys`.
    public static double[] ys(YCoords ys, int st, double[] shape) {
        if (shape == UNSTABLE_BOTTOM) return ys.scaffoldingBottom();
        if (shape == SNOW_FALLING) return ys.snowFalling();
        Object[] t = ys.states();
        double[] c = shape == CUBE || st >= t.length ? null
                   : (double[]) t[st];
        return c == null ? ys.block() : c;
    }

    /// Returns the boxes of `st` at `x`, `y`, `z`, relative to the
    /// cell, that a body with its bottom at `bottom` and the `flags`
    /// meets, or null when none.
    public static double[] shape(Object[] shapes, byte[] kinds, int st,
            int x, int y, int z, double bottom, int flags) {
        int kind = kind(kinds, st);
        switch (kind) {
            case SCAFFOLDING, SCAFFOLDING_HANGING -> {
                boolean still = (flags & DESCENDING) == 0;
                if (still && above(bottom, y, 1.0)) return boxes(shapes, st);
                return kind == SCAFFOLDING_HANGING
                    && above(bottom, y, 0.0) ? UNSTABLE_BOTTOM : null;
            }
            case POWDER_SNOW -> {
                if ((flags & FALLING) != 0) return SNOW_FALLING;
                if ((flags & FALLING_BLOCK) != 0) return CUBE;
                return (flags & WALKER) != 0 && above(bottom, y, 1.0)
                    && (flags & DESCENDING) == 0 ? CUBE : null;
            }
            case OFFSET_QUARTER -> {
                double[] b = boxes(shapes, st);
                return b == null ? null : moved(b, 0.25F, x, z);
            }
            case OFFSET_EIGHTH -> {
                double[] b = boxes(shapes, st);
                return b == null ? null : moved(b, 0.125F, x, z);
            }
            default -> {
                return boxes(shapes, st);
            }
        }
    }
}
