package collider.world.space;

/// A cell of one path search with its scores and its place in the
/// open set.
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
    boolean closed;
    PathNode came;
    int kind;

    PathNode(long x, long y, long z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    static double fl(double v) {
        return (float) v;
    }

    double malus() {
        return malus;
    }

    void setMalus(double m) {
        malus = fl(m);
    }

    /// Returns the path type of the cell as its index among the path
    /// types.
    public int kind() {
        return kind;
    }

    double distTo(long x, long y, long z) {
        long dx = x - this.x, dy = y - this.y, dz = z - this.z;
        return fl(Math.sqrt(dx * dx + dy * dy + dz * dz));
    }

    double manhattan(long x, long y, long z) {
        return fl(Math.abs(x - this.x) + Math.abs(y - this.y) + Math.abs(z - this.z));
    }
}
