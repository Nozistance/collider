package collider.world;

/// The collision boxes of a block state as a body meets them.
/// Scaffolding and powder snow depend on the body, bamboo and the
/// speleothems on the position of the block.
public final class Collision {

    public static final double[] CUBE = {0, 0, 0, 1, 1, 1};

    private static final double[] UNSTABLE_BOTTOM = {0, 0, 0, 1, 0.125, 1};

    private static final double[] SNOW_FALLING = {0, 0, 0, 1, 0.9F, 1};

    private static final double EPSILON = 1.0E-7;

    /// The kind of a block state whose shape is its boxes.
    public static final byte PLAIN = 0;

    public static final byte SCAFFOLDING = 1;

    public static final byte SCAFFOLDING_HANGING = 2;

    public static final byte POWDER_SNOW = 3;

    /// The kind of a state offset by up to a quarter of a block.
    public static final byte OFFSET_QUARTER = 4;

    /// The kind of a state offset by up to an eighth of a block.
    public static final byte OFFSET_EIGHTH = 5;

    public static final byte MOVING = 6;

    public static final int DESCENDING = 1;

    /// The flag of a body that falls more than 2.5 blocks.
    public static final int FALLING = 2;

    /// The flag of a body that walks on powder snow.
    public static final int WALKER = 4;

    public static final int FALLING_BLOCK = 8;

    public static final int NOCLIP = 4;

    /// Returns the boxes of st at the origin as an empty context
    /// meets them, or null when it has none. A state past the end of
    /// shapes counts as a full cube.
    public static double[] boxes(Object[] shapes, int st) {
        if (st <= 0) {
            return null;
        }
        if (st >= shapes.length) {
            return CUBE;
        }
        double[] b = (double[]) shapes[st];
        return b.length == 0 ? null : b;
    }

    public static int withNoclip(int flags, int direction) {
        return (flags & ~(7 << NOCLIP)) | ((direction + 1) << NOCLIP);
    }

    public static int noclip(int flags) {
        int v = (flags >>> NOCLIP) & 7;
        return v == 0 ? -1 : v - 1;
    }

    /// Returns the kind of st, PLAIN for a state that kinds lacks.
    public static int kind(byte[] kinds, int st) {
        return st > 0 && st < kinds.length ? kinds[st] : PLAIN;
    }

    /// Returns true when some body meets boxes of st.
    public static boolean mayCollide(Object[] shapes, byte[] kinds, int st) {
        return boxes(shapes, st) != null || kind(kinds, st) == POWDER_SNOW;
    }

    private static boolean above(double bottom, int y, double top) {
        return bottom > y + top - 1.0E-5F;
    }

    private static long seed(int x, int z) {
        int xs = x * 3129871;
        long seed = (long) xs ^ (long) z * 116129781L;
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

    private static double at(int i, double lo, double hi) {
        return i == 0 ? lo : hi;
    }

    /// Returns true when the span from f0 to f1 of a shape and the
    /// span from s0 to s1 of a body meet on one axis once their
    /// coordinates merge. A coordinate within 1.0E-7 above the last
    /// one kept joins it.
    public static boolean joins(double f0, double f1, double s0, double s1) {
        if (f1 - f0 < EPSILON || s1 - s0 < EPSILON) {
            return false;
        }
        if (f1 < s0 - EPSILON || s1 < f0 - EPSILON) {
            return false;
        }
        if (f0 == s0 && f1 == s1) {
            return true;
        }
        int fi = 0, si = 0, cf = -1, cs = -1;
        boolean open = false;
        double last = Double.NaN;
        while (fi < 2 || si < 2) {
            boolean outF = fi >= 2, outS = si >= 2;
            boolean first = !outF && (outS || at(fi, f0, f1) < at(si, s0, s1) + EPSILON);
            double v = first ? at(fi++, f0, f1) : at(si++, s0, s1);
            if (first ? si == 0 || outS : fi == 0 || outF) {
                continue;
            }
            if (!(last >= v - EPSILON)) {
                if (open && cf == 0 && cs == 0) {
                    return true;
                }
                open = true;
                last = v;
            }
            cf = fi - 1;
            cs = si - 1;
        }
        return false;
    }

    private static boolean joins(double[] a, int o, double[] box) {
        return joins(a[o], a[o + 3], box[0], box[3])
                && joins(a[o + 1], a[o + 4], box[1], box[4])
                && joins(a[o + 2], a[o + 5], box[2], box[5]);
    }

    private static boolean unit(double lo, double hi) {
        return lo == Math.floor(lo) && hi == lo + 1.0;
    }

    private static boolean cell(double[] a, int o) {
        return unit(a[o], a[o + 3])
                && unit(a[o + 1], a[o + 4])
                && unit(a[o + 2], a[o + 5]);
    }

    /// Returns true when the block box at offset o of a meets the
    /// body box. A whole cell meets by any overlap, any other box as
    /// joins finds it.
    public static boolean meets(double[] a, int o, double[] box) {
        if (!cell(a, o)) {
            return joins(a, o, box);
        }
        return a[o + 3] > box[0]
                && box[3] > a[o]
                && a[o + 4] > box[1]
                && box[4] > a[o + 1]
                && a[o + 5] > box[2]
                && box[5] > a[o + 2];
    }

    private static final int[] STEP_X = {0, 0, 0, 0, -1, 1};

    private static final int[] STEP_Y = {-1, 1, 0, 0, 0, 0};

    private static final int[] STEP_Z = {0, 0, -1, 1, 0, 0};

    private static double[] shift(double[] b, double dx, double dy, double dz) {
        if (b == null || (dx == 0.0 && dy == 0.0 && dz == 0.0)) {
            return b;
        }
        double[] m = b.clone();
        for (int k = 0; k < m.length; k += 6) {
            m[k] += dx;
            m[k + 1] += dy;
            m[k + 2] += dz;
            m[k + 3] += dx;
            m[k + 4] += dy;
            m[k + 5] += dz;
        }
        return m;
    }

    private static double[] join(double[] a, double[] b) {
        if (a == null) {
            return b;
        }
        if (b == null) {
            return a;
        }
        double[] m = new double[a.length + b.length];
        System.arraycopy(a, 0, m, 0, a.length);
        System.arraycopy(b, 0, m, a.length, b.length);
        return m;
    }

    private static boolean covers(double lo, double hi) {
        return lo <= EPSILON && hi >= 1.0 - EPSILON;
    }

    /// Returns true when one box of boxes covers the whole unit face
    /// direction. direction is a face index.
    public static boolean faceFull(double[] boxes, int direction) {
        if (boxes == null) {
            return false;
        }
        int axis = direction < 2 ? 1 : (direction < 4 ? 2 : 0);
        boolean positive = (direction & 1) == 1;
        int u = (axis + 1) % 3;
        int v = (axis + 2) % 3;
        for (int k = 0; k < boxes.length; k += 6) {
            double lo = boxes[k + axis];
            double hi = boxes[k + axis + 3];
            boolean touch = positive ? hi >= 1.0 - EPSILON : lo <= EPSILON;
            if (touch && covers(boxes[k + u], boxes[k + u + 3])
                    && covers(boxes[k + v], boxes[k + v + 3])) {
                return true;
            }
        }
        return false;
    }

    /// Returns the boxes of moving record packed, relative to its
    /// cell, or null when it has none. A facing above 5 or a progress
    /// above 2 throws.
    public static double[] moving(Object[] shapes, long packed, int flags) {
        if (packed == 0L) {
            return null;
        }
        int shape = (int) (packed & 0xFFFFFL);
        int base = (int) ((packed >>> 20) & 0xFFFFFL);
        int facing = (int) ((packed >>> 40) & 7L);
        int progress = (int) ((packed >>> 43) & 3L);
        boolean extending = ((packed >>> 45) & 1L) != 0;
        if (facing > 5 || progress > 2) {
            throw new IllegalArgumentException("moving " + packed);
        }
        int movement = extending ? facing : (facing ^ 1);
        double[] baseBoxes = base == 0 ? null : boxes(shapes, base);
        if (progress < 2 && noclip(flags) == movement) {
            return baseBoxes;
        }
        float p = (float) progress * 0.5f;
        float ext = extending ? p - 1.0f : 1.0f - p;
        double dx = (double) ((float) STEP_X[facing] * ext);
        double dy = (double) ((float) STEP_Y[facing] * ext);
        double dz = (double) ((float) STEP_Z[facing] * ext);
        return join(baseBoxes, shift(boxes(shapes, shape), dx, dy, dz));
    }

    /// Returns the y coordinates in ys of the boxes that the methods
    /// shape or boxes returned for st.
    public static double[] ys(YCoords ys, int st, double[] shape) {
        if (shape == UNSTABLE_BOTTOM) {
            return ys.scaffoldingBottom();
        }
        if (shape == SNOW_FALLING) {
            return ys.snowFalling();
        }
        Object[] t = ys.states();
        double[] c = shape == CUBE || st >= t.length ? null : (double[]) t[st];
        return c == null ? ys.block() : c;
    }

    /// Returns the boxes of st at x, y, z, relative to the cell, that
    /// a body with its bottom at bottom and these flags meets, or
    /// null when none.
    public static double[] shape(
            Object[] shapes,
            byte[] kinds,
            int st,
            int x,
            int y,
            int z,
            double bottom,
            int flags
    ) {
        int kind = kind(kinds, st);
        switch (kind) {
            case SCAFFOLDING, SCAFFOLDING_HANGING -> {
                boolean still = (flags & DESCENDING) == 0;
                if (still && above(bottom, y, 1.0)) {
                    return boxes(shapes, st);
                }
                boolean hangs = kind == SCAFFOLDING_HANGING && above(bottom, y, 0.0);
                return hangs ? UNSTABLE_BOTTOM : null;
            }
            case POWDER_SNOW -> {
                if ((flags & FALLING) != 0) {
                    return SNOW_FALLING;
                }
                if ((flags & FALLING_BLOCK) != 0) {
                    return CUBE;
                }
                boolean stands = (flags & WALKER) != 0
                        && above(bottom, y, 1.0)
                        && (flags & DESCENDING) == 0;
                return stands ? CUBE : null;
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
