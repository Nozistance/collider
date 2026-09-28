package collider.game.mob;

/// The bodies in one cell of the push grid. Index `i` of each
/// component holds the same body.
public record PushCell(long[] eids, double[] halfs, double[] heights,
                       double[] xs, double[] ys, double[] zs) {}
