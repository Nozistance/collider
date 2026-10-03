package collider.game.mob;

import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentVector;
import java.util.Arrays;

/// The bodies of a level that shove each other, in the order they
/// were added, with what the push grid of each island takes.
public final class Bodies {

    private Object[] entries = new Object[16];
    private long[] eids = new long[16], came = new long[16], ranks = new long[16];
    private double[] halfs = new double[16], heights = new double[16];
    private double[] xs = new double[16], ys = new double[16], zs = new double[16];
    private boolean[] ticking = new boolean[16];
    private int n;

    /// Adds to `b` the body `eid` of map entry `entry`, of half width
    /// `half` and height `height` at `x`, `y`, `z`, come into its
    /// section at tick `came` and rank `rank`, and whether it ticks,
    /// and returns `b`.
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
    public static Bodies joined(Bodies b, Bodies o) {
        int m = b.n + o.n;
        if (m > b.eids.length) b.grow(Math.max(m, 2 * b.n));
        System.arraycopy(o.entries, 0, b.entries, b.n, o.n);
        System.arraycopy(o.eids, 0, b.eids, b.n, o.n);
        System.arraycopy(o.halfs, 0, b.halfs, b.n, o.n);
        System.arraycopy(o.heights, 0, b.heights, b.n, o.n);
        System.arraycopy(o.xs, 0, b.xs, b.n, o.n);
        System.arraycopy(o.ys, 0, b.ys, b.n, o.n);
        System.arraycopy(o.zs, 0, b.zs, b.n, o.n);
        System.arraycopy(o.came, 0, b.came, b.n, o.n);
        System.arraycopy(o.ranks, 0, b.ranks, b.n, o.n);
        System.arraycopy(o.ticking, 0, b.ticking, b.n, o.n);
        b.n = m;
        return b;
    }

    /// Returns the indices in `b` of its bodies in groups, as
    /// [Islands#of] groups them by the cells of the push grid.
    public static int[][] islands(Bodies b) {
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
        if (g.length == 0) return PersistentVector.EMPTY;
        Object[] out = new Object[g.length];
        for (int k = 0; k < g.length; k++) out[k] = b.entries[g[k]];
        return LazilyPersistentVector.createOwning(out);
    }

    private long[] longs(long[] a, int[] g) {
        long[] out = new long[g.length];
        for (int k = 0; k < g.length; k++) out[k] = a[g[k]];
        return out;
    }

    private double[] doubles(double[] a, int[] g) {
        double[] out = new double[g.length];
        for (int k = 0; k < g.length; k++) out[k] = a[g[k]];
        return out;
    }

    /// Returns the push grid of the bodies of `b` at indices `g`,
    /// ascending by id, as they stand now.
    public static PushGrid grid(Bodies b, int[] g) {
        return new PushGrid(
                b.longs(b.eids, g),
                b.doubles(b.halfs, g),
                b.doubles(b.heights, g),
                b.doubles(b.xs, g),
                b.doubles(b.ys, g),
                b.doubles(b.zs, g),
                b.longs(b.came, g),
                b.longs(b.ranks, g)
        );
    }

    /// Returns the places by id of the bodies of `b` at indices `g`.
    public static Slots slots(Bodies b, int[] g) {
        return Slots.of(b.longs(b.eids, g));
    }

    /// Returns whether each body of `b` at indices `g` ticks.
    public static boolean[] ticking(Bodies b, int[] g) {
        boolean[] out = new boolean[g.length];
        for (int k = 0; k < g.length; k++) out[k] = b.ticking[g[k]];
        return out;
    }
}
