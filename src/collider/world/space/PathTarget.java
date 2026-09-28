package collider.world.space;

/// A goal cell of one path search, with the node that came closest
/// to it so far.
public final class PathTarget {

    public final long x;

    public final long y;

    public final long z;

    private double best = Float.MAX_VALUE;
    private PathNode node;

    /// Makes the goal cell `x`, `y`, `z` that no node came near yet.
    public PathTarget(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    /// Returns the node that came closest, null when none did.
    public PathNode node() {
        return node;
    }

    void offer(double h, PathNode n) {
        if (h < best) {
            best = h;
            node = n;
        }
    }
}
