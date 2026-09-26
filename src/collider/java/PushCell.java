package collider.java;

/// The bodies one cell of the push grid holds: id, position, half
/// width and height of each, by index.
public final class PushCell {

    public final long[] eids;
    public final double[] xs, ys, zs;
    public final double[] halfs, heights;

    public PushCell(long[] eids, double[] xs, double[] ys, double[] zs,
                    double[] halfs, double[] heights) {
        this.eids = eids;
        this.xs = xs;
        this.ys = ys;
        this.zs = zs;
        this.halfs = halfs;
        this.heights = heights;
    }
}
