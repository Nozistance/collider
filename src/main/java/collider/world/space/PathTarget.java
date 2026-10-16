package collider.world.space;

/// A goal cell of one path search, with the node that came closest
/// to it so far, null while none did.
public final class PathTarget {

    public final long x;
    public final long y;
    public final long z;
    private double best = Float.MAX_VALUE;
    PathNode node;

    public PathTarget(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    void offer(double h, PathNode n) {
        if (h < best) {
            best = h;
            node = n;
        }
    }
}
