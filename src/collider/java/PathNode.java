package collider.java;

import clojure.lang.Keyword;

/// A cell of one path search, with its scores and its place in the
/// open set. The scores hold the float values the game would hold.
public final class PathNode {

    private static final Object BLOCKED = Keyword.intern("blocked");

    /// The block x of the cell.
    public final long x;

    /// The block y of the cell.
    public final long y;

    /// The block z of the cell.
    public final long z;

    double g;
    double h;
    double f;
    double walked;
    private double malus;
    int heapIdx = -1;
    private boolean closed;
    PathNode came;
    private Object type = BLOCKED;

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

    /// Sets the cost the path type of the cell adds to `m`.
    public void setMalus(double m) {
        malus = fl(m);
    }

    /// Returns the path type of the cell.
    public Object type() {
        return type;
    }

    /// Sets the path type of the cell to `t`.
    public void setType(Object t) {
        type = t;
    }

    /// Returns true when the search is done with the node.
    public boolean closed() {
        return closed;
    }

    /// Marks the search done with the node.
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

    /// Returns the distance to the cell `x`, `y`, `z` along the
    /// axes.
    public double manhattan(long x, long y, long z) {
        return fl(Math.abs(x - this.x) + Math.abs(y - this.y)
                  + Math.abs(z - this.z));
    }
}
