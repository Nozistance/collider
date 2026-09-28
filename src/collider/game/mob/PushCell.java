package collider.game.mob;

/// The bodies in one cell of the push grid. Index `i` of each
/// component holds the same body.
///
/// @param eids The entity ids.
/// @param halfs The half widths.
/// @param heights The heights.
/// @param xs The x coordinates.
/// @param ys The y coordinates.
/// @param zs The z coordinates.
public record PushCell(long[] eids, double[] halfs, double[] heights,
                       double[] xs, double[] ys, double[] zs) {}
