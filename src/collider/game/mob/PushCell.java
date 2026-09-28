package collider.game.mob;

import java.util.Arrays;

/// The bodies in one cell of the push grid. Index `i` of each
/// component holds the same body. The eids ascend.
public record PushCell(long[] eids, double[] halfs, double[] heights,
                       double[] xs, double[] ys, double[] zs) {

    /// Returns this cell without the body `eid`, or a copy of it
    /// when no such body is here.
    public PushCell without(long eid) {
        int i = Arrays.binarySearch(eids, eid);
        if (i < 0) return copy();
        int n = eids.length - 1;
        return new PushCell(drop(eids, i, n), drop(halfs, i, n),
                            drop(heights, i, n), drop(xs, i, n),
                            drop(ys, i, n), drop(zs, i, n));
    }

    /// Returns this cell with the body `eid` at `x`, `y`, `z`, put
    /// in by id or replaced in place when already here.
    public PushCell with(long eid, double half, double height,
                         double x, double y, double z) {
        int i = Arrays.binarySearch(eids, eid);
        if (i >= 0) return replaced(i, eid, half, height, x, y, z);
        i = -i - 1;
        int n = eids.length + 1;
        return new PushCell(put(eids, i, n, eid), put(halfs, i, n, half),
                            put(heights, i, n, height), put(xs, i, n, x),
                            put(ys, i, n, y), put(zs, i, n, z));
    }

    private PushCell replaced(int i, long eid, double half,
                              double height, double x, double y, double z) {
        PushCell c = copy();
        c.eids[i] = eid;
        c.halfs[i] = half;
        c.heights[i] = height;
        c.xs[i] = x;
        c.ys[i] = y;
        c.zs[i] = z;
        return c;
    }

    private PushCell copy() {
        return new PushCell(eids.clone(), halfs.clone(), heights.clone(),
                            xs.clone(), ys.clone(), zs.clone());
    }

    private static long[] drop(long[] a, int i, int n) {
        long[] r = new long[n];
        System.arraycopy(a, 0, r, 0, i);
        System.arraycopy(a, i + 1, r, i, n - i);
        return r;
    }

    private static double[] drop(double[] a, int i, int n) {
        double[] r = new double[n];
        System.arraycopy(a, 0, r, 0, i);
        System.arraycopy(a, i + 1, r, i, n - i);
        return r;
    }

    private static long[] put(long[] a, int i, int n, long v) {
        long[] r = new long[n];
        System.arraycopy(a, 0, r, 0, i);
        r[i] = v;
        System.arraycopy(a, i, r, i + 1, n - i - 1);
        return r;
    }

    private static double[] put(double[] a, int i, int n, double v) {
        double[] r = new double[n];
        System.arraycopy(a, 0, r, 0, i);
        r[i] = v;
        System.arraycopy(a, i, r, i + 1, n - i - 1);
        return r;
    }
}
