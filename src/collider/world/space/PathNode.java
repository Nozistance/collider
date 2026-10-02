package collider.world.space;

/// A cell of one path search with its scores and its place in the
/// open set. The scores hold float values.
public final class PathNode {

    public final long x;

    public final long y;

    public final long z;

    double g;
    double h;
    double f;
    double walked;
    private double malus;
    int heapIdx = -1;
    private boolean closed;
    PathNode came;
    int kind;

    /// Makes the node of the cell `x`, `y`, `z`, of type blocked and
    /// out of the open set.
    public PathNode(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    static double fl(double v) {
        return (float) v;
    }

    /// Returns the cost the path type of the cell adds.
    public double malus() {
        return malus;
    }

    public void setMalus(double m) {
        malus = fl(m);
    }

    /// Returns the path type of the cell, as its place in the order
    /// the game declares them.
    public int kind() {
        return kind;
    }

    public void setKind(int t) {
        kind = t;
    }

    /// Returns true when the search is done with the node.
    public boolean closed() {
        return closed;
    }

    public void close() {
        closed = true;
    }

    /// Returns the node the best path came from, null for the first.
    public PathNode came() {
        return came;
    }

    /// Returns the straight distance to the cell `x`, `y`, `z`.
    public double distTo(long x, long y, long z) {
        long dx = x - this.x, dy = y - this.y, dz = z - this.z;
        return fl(Math.sqrt((double) (dx * dx + dy * dy + dz * dz)));
    }

    public double manhattan(long x, long y, long z) {
        return fl(Math.abs(x - this.x) + Math.abs(y - this.y) + Math.abs(z - this.z));
    }
}
