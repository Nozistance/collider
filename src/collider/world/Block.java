package collider.world;

import clojure.lang.IFn;
import clojure.lang.RT;

/// Lookups by block state over the `BlockTables`, and the loops
/// that build the tables.
///
/// A state outside the tables is unknown: it answers false, null
/// or the default of the table.
public final class Block {

    private static boolean is(boolean[] a, long st) {
        return st >= 0 && st < a.length && a[(int) st];
    }

    private static Object at(Object[] a, long st) {
        return st >= 0 && st < a.length ? a[(int) st] : null;
    }

    private static boolean bit(byte[] a, long st, long b) {
        return st >= 0 && st < a.length && (a[(int) st] & b) != 0;
    }

    /// Returns the type of `st`, or null.
    public static Object type(BlockTables t, long st) {
        return at(t.types(), st);
    }

    /// Returns the block of `st`, or null.
    public static Object name(BlockTables t, long st) {
        return at(t.names(), st);
    }

    /// Returns the shape kind of `st`, or null.
    public static Object shape(BlockTables t, long st) {
        return at(t.shapes(), st);
    }

    /// Returns true when `st` breaks without support.
    public static boolean needsSupport(BlockTables t, long st) {
        return is(t.needsSupport(), st);
    }

    /// Returns true when `st` hangs on a neighbour.
    public static boolean attached(BlockTables t, long st) {
        return is(t.attached(), st);
    }

    /// Returns true when `st` a placement replaces.
    public static boolean replaceable(BlockTables t, long st) {
        return is(t.replaceable(), st);
    }

    /// Returns true when `st` is a liquid.
    public static boolean liquid(BlockTables t, long st) {
        return is(t.liquid(), st);
    }

    /// Returns true when `st` holds water.
    public static boolean waterlogged(BlockTables t, long st) {
        return is(t.waterlogged(), st);
    }

    /// Returns true when `st` falls.
    public static boolean falls(BlockTables t, long st) {
        return is(t.falls(), st);
    }

    /// Returns true for air and for a state in the replaceable tag.
    public static boolean canBeReplaced(BlockTables t, long st) {
        return st == 0 || is(t.canBeReplaced(), st);
    }

    /// Returns true when `st` stops a body.
    public static boolean solid(BlockTables t, long st) {
        return is(t.solid(), st);
    }

    /// Returns true when `st` is solid by the legacy rule, never air.
    public static boolean legacySolid(BlockTables t, long st) {
        return st > 0 && is(t.legacySolid(), st);
    }

    /// Returns true when `st` collides as a full cube.
    public static boolean fullCube(BlockTables t, long st) {
        return is(t.fullCube(), st);
    }

    /// Returns true when `st` blocks motion.
    public static boolean blocksMotion(BlockTables t, long st) {
        return is(t.blocksMotion(), st);
    }

    /// Returns true when `st` occludes light by its shape.
    public static boolean useShape(BlockTables t, long st) {
        return is(t.useShape(), st);
    }

    /// Returns true when `st` can occlude.
    public static boolean canOcclude(BlockTables t, long st) {
        return is(t.canOcclude(), st);
    }

    /// Returns the light that `st` takes, 15 for an unknown state.
    public static long dampening(BlockTables t, long st) {
        int[] a = t.dampening();
        return st >= 0 && st < a.length ? a[(int) st] : 15;
    }

    /// Returns the light that `st` gives.
    public static long emission(BlockTables t, long st) {
        int[] a = t.emission();
        return st >= 0 && st < a.length ? a[(int) st] : 0;
    }

    /// Returns the blast resistance of `st`, 3.0 for an unknown
    /// state.
    public static double resist(BlockTables t, long st) {
        double[] a = t.resist();
        return st >= 0 && st < a.length ? a[(int) st] : 3.0;
    }

    /// Returns true when the flags of `st` have a bit of `mask`.
    public static boolean flag(BlockTables t, long st, long mask) {
        return bit(t.flags(), st, mask);
    }

    /// Returns true when face `d` of `st` holds things.
    public static boolean sturdy(BlockTables t, long st, long d) {
        return bit(t.sturdy(), st, 1L << d);
    }

    /// Returns true when face `d` of `st` holds things rigidly.
    public static boolean sturdyRigid(BlockTables t, long st, long d) {
        return bit(t.sturdyRigid(), st, 1L << d);
    }

    /// Returns true when face `d` of `st` holds things at its
    /// center.
    public static boolean sturdyCenter(BlockTables t, long st,
            long d) {
        return bit(t.sturdyCenter(), st, 1L << d);
    }

    private static long[] face(BlockTables t, long st, int d) {
        long n = t.faces().length / 6;
        return st >= 0 && st < n
                ? (long[]) t.faces()[(int) st * 6 + d] : null;
    }

    private static long[] side(BlockTables t, long st, int d) {
        return st >= 0 && st < t.touch().length
                && (t.touch()[(int) st] & (1 << d)) != 0
                ? face(t, st, d) : null;
    }

    private static boolean full(long[] m) {
        return m[0] == -1 && m[1] == -1 && m[2] == -1 && m[3] == -1;
    }

    private static boolean covers(long[] a, long[] b) {
        return (a[0] | b[0]) == -1 && (a[1] | b[1]) == -1
                && (a[2] | b[2]) == -1 && (a[3] | b[3]) == -1;
    }

    private static boolean seals(long[] a, long[] b) {
        if (a == null && b == null) return false;
        if (a == null) return full(b);
        if (b == null) return full(a);
        return covers(a, b);
    }

    /// Returns true when the face of `from` along `d` and the
    /// opposite face of `to` seal: no light goes through.
    public static boolean occludes(BlockTables t, long from, long to,
            long d) {
        return seals(face(t, from, (int) d), face(t, to, (int) d ^ 1));
    }

    /// Returns the light cost of crossing from `from` into `to`
    /// along `d`: 16 when the faces that the shapes touch seal,
    /// `simple` otherwise.
    public static long dampeningInto(BlockTables t, long from, long to,
            long d, long simple) {
        long[] a = side(t, from, (int) d);
        long[] b = side(t, to, (int) d ^ 1);
        return seals(a, b) ? 16 : simple;
    }

    /// Returns the face of a shape as 16 by 16 bits in four longs.
    /// `boxes` holds four numbers for each box: the low u and v
    /// and the high u and v, in pixels.
    public static long[] faceMask(long[] boxes) {
        long[] m = new long[4];
        for (int i = 0; i + 3 < boxes.length; i += 4) {
            for (long v = boxes[i + 1]; v < boxes[i + 3]; v++) {
                for (long u = boxes[i]; u < boxes[i + 2]; u++) {
                    long b = 16 * v + u;
                    m[(int) (b >> 6)] |= 1L << b;
                }
            }
        }
        return m;
    }

    /// Returns the table true for each state whose `pred` is
    /// truthy. `pred` takes the state, its type and its block; a
    /// state without a type is false.
    public static boolean[] table(Object[] types, Object[] names,
            IFn pred) {
        boolean[] a = new boolean[types.length];
        for (int i = 0; i < a.length; i++) {
            if (types[i] != null) {
                a[i] = RT.booleanCast(pred.invoke((long) i, types[i],
                                                  names[i]));
            }
        }
        return a;
    }

    /// Returns the table true for each state but air whose
    /// collision `shapes` entry is null: a full cube.
    public static boolean[] fullCubes(Object[] shapes) {
        boolean[] a = new boolean[shapes.length];
        for (int i = 1; i < a.length; i++) a[i] = shapes[i] == null;
        return a;
    }

    /// Returns the span of `boxes` along axis `lo`, in blocks.
    private static double span(double[] boxes, int lo) {
        double a = Double.POSITIVE_INFINITY;
        double b = Double.NEGATIVE_INFINITY;
        for (int i = 0; i < boxes.length; i += 6) {
            a = Math.min(a, boxes[i + lo]);
            b = Math.max(b, boxes[i + lo + 3]);
        }
        return b - a;
    }

    /// Returns the table true for each state solid by the legacy
    /// rule: its boxes span on average at least 0.729 of a block,
    /// or the full height. `boxes` holds the collision boxes of
    /// each state, six doubles each, in blocks.
    public static boolean[] legacySolids(Object[] boxes) {
        boolean[] a = new boolean[boxes.length];
        for (int i = 0; i < a.length; i++) {
            double[] b = (double[]) boxes[i];
            if (b.length > 0) {
                double ys = span(b, 1);
                double avg = (span(b, 0) + ys + span(b, 2)) / 3.0;
                a[i] = avg >= 0.7291666666666666 || ys >= 1.0;
            }
        }
        return a;
    }
}
