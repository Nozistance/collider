package collider.game.mob;

import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentVector;
import java.util.ArrayList;
import java.util.Arrays;

/// The bodies of a level that shove each other, in the order they
/// were added, with what the push grid of each island takes.
public final class Bodies {

    private static final int START = 64;

    private Object[] entries = new Object[START];
    private long[] eids = new long[START], came = new long[START], ranks = new long[START];
    private double[] halfs = new double[START], heights = new double[START];
    private double[] xs = new double[START], ys = new double[START], zs = new double[START];
    private boolean[] ticking = new boolean[START];
    private int n;
    private ArrayList<Bodies> rest;

    /// Adds body `eid` to `b` and returns `b`. The tick `came` and the
    /// `rank` place it among the bodies of its section.
    public static Bodies add(
            Bodies b,
            Object entry,
            long eid,
            double half,
            double height,
            double x,
            double y,
            double z,
            long came,
            long rank,
            boolean ticking
    ) {
        if (b.n == b.eids.length) b.grow(2 * b.n);
        int i = b.n++;
        b.entries[i] = entry;
        b.eids[i] = eid;
        b.halfs[i] = half;
        b.heights[i] = height;
        b.xs[i] = x;
        b.ys[i] = y;
        b.zs[i] = z;
        b.came[i] = came;
        b.ranks[i] = rank;
        b.ticking[i] = ticking;
        return b;
    }

    private void grow(int size) {
        entries = Arrays.copyOf(entries, size);
        eids = Arrays.copyOf(eids, size);
        halfs = Arrays.copyOf(halfs, size);
        heights = Arrays.copyOf(heights, size);
        xs = Arrays.copyOf(xs, size);
        ys = Arrays.copyOf(ys, size);
        zs = Arrays.copyOf(zs, size);
        came = Arrays.copyOf(came, size);
        ranks = Arrays.copyOf(ranks, size);
        ticking = Arrays.copyOf(ticking, size);
    }

    /// Adds to `b` the bodies of `o` after its own and returns `b`.
    /// No body may be added to `o` after.
    public static Bodies joined(Bodies b, Bodies o) {
        if (b.rest == null) b.rest = new ArrayList<>();
        b.rest.add(o);
        if (o.rest != null) {
            b.rest.addAll(o.rest);
            o.rest = null;
        }
        return b;
    }

    private Bodies packed() {
        if (rest == null) return this;
        int m = n;
        for (Bodies o : rest) m += o.n;
        grow(m);
        for (Bodies o : rest) {
            System.arraycopy(o.entries, 0, entries, n, o.n);
            System.arraycopy(o.eids, 0, eids, n, o.n);
            System.arraycopy(o.halfs, 0, halfs, n, o.n);
            System.arraycopy(o.heights, 0, heights, n, o.n);
            System.arraycopy(o.xs, 0, xs, n, o.n);
            System.arraycopy(o.ys, 0, ys, n, o.n);
            System.arraycopy(o.zs, 0, zs, n, o.n);
            System.arraycopy(o.came, 0, came, n, o.n);
            System.arraycopy(o.ranks, 0, ranks, n, o.n);
            System.arraycopy(o.ticking, 0, ticking, n, o.n);
            n += o.n;
        }
        rest = null;
        return this;
    }

    /// Returns the indices in `b` of its bodies, in the groups whose
    /// cells of the push grid touch.
    public static int[][] islands(Bodies b) {
        b.packed();
        long[] cells = new long[b.n];
        for (int i = 0; i < b.n; i++) {
            long cx = Math.floorDiv((long) Math.floor(b.xs[i]), 4);
            long cz = Math.floorDiv((long) Math.floor(b.zs[i]), 4);
            cells[i] = Islands.key(cx, cz);
        }
        return Islands.of(Arrays.copyOf(b.eids, b.n), cells);
    }

    /// Returns the map entries of the bodies of `b` at indices `g`.
    public static Object entries(Bodies b, int[] g) {
        b.packed();
        if (g.length == 0) return PersistentVector.EMPTY;
        Object[] out = new Object[g.length];
        for (int k = 0; k < g.length; k++) out[k] = b.entries[g[k]];
        return LazilyPersistentVector.createOwning(out);
    }

    private static long[] longs(long[] a, int[] g) {
        long[] out = new long[g.length];
        for (int k = 0; k < g.length; k++) out[k] = a[g[k]];
        return out;
    }

    private static double[] doubles(double[] a, int[] g) {
        double[] out = new double[g.length];
        for (int k = 0; k < g.length; k++) out[k] = a[g[k]];
        return out;
    }

    /// Returns the push grid of the bodies of `b` at indices `g`,
    /// ascending by id, as they stand now.
    public static PushGrid grid(Bodies b, int[] g) {
        b.packed();
        return new PushGrid(
                longs(b.eids, g),
                doubles(b.halfs, g),
                doubles(b.heights, g),
                doubles(b.xs, g),
                doubles(b.ys, g),
                doubles(b.zs, g),
                longs(b.came, g),
                longs(b.ranks, g),
                ticking(b, g)
        );
    }

    /// Returns the places by id of the bodies of `b` at indices `g`.
    public static Slots slots(Bodies b, int[] g) {
        b.packed();
        return Slots.of(longs(b.eids, g));
    }

    /// Returns whether each body of `b` at indices `g` ticks.
    public static boolean[] ticking(Bodies b, int[] g) {
        b.packed();
        boolean[] out = new boolean[g.length];
        for (int k = 0; k < g.length; k++) out[k] = b.ticking[g[k]];
        return out;
    }
}
