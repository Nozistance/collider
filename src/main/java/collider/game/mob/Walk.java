package collider.game.mob;

/// The numeric core of walking a path: when a mob counts a node as
/// reached and how long it may take to get there.
public final class Walk {

    private static final double MAX_VERTICAL = 1.0;

    /// Returns true when a mob `width` wide at `x`, `y`, `z` stands
    /// close enough to the node `nx`, `ny`, `nz` to take the next.
    public static boolean closeEnough(
            double x,
            double y,
            double z,
            long nx,
            long ny,
            long nz,
            double width
    ) {
        double maxd = width > 0.75 ? width / 2.0 : 0.75 - width / 2.0;
        return Math.abs(x - (nx + 0.5)) < maxd
                && Math.abs(z - (nz + 0.5)) < maxd
                && Math.abs(y - (double) ny) < MAX_VERTICAL;
    }

    /// Returns true when a mob at `x`, `y`, `z` near the node `c`
    /// has the next node `n` behind it, so it may skip `c`.
    public static boolean turnedBack(
            double x,
            double y,
            double z,
            long cx0,
            long cy0,
            long cz0,
            long nx0,
            long ny0,
            long nz0
    ) {
        double cx = (cx0 + 0.5) - x, cy = (double) cy0 - y;
        double cz = (cz0 + 0.5) - z;
        double cs = cx * cx + cy * cy + cz * cz;
        if (!(cs < 4.0)) return false;
        double nx = (nx0 + 0.5) - x, ny = (double) ny0 - y;
        double nz = (nz0 + 0.5) - z;
        double ns = nx * nx + ny * ny + nz * nz;
        double cl = Math.sqrt(cs), nl = Math.sqrt(ns);
        return (ns < cs || cs < 0.5)
                && (nx / nl) * (cx / cl) + (ny / nl) * (cy / cl) + (nz / nl) * (cz / cl) < 0.0;
    }

    /// Returns the ticks a mob at `x`, `y`, `z` walking at `speed`
    /// is given to reach the node `nx`, `ny`, `nz`, 0 when it stands.
    public static double timeout(
            double speed,
            double x,
            double y,
            double z,
            long nx,
            long ny,
            long nz
    ) {
        if (!(speed > 0.0)) return 0.0;
        double dx = x - (nx + 0.5), dy = y - (double) ny;
        double dz = z - (nz + 0.5);
        return Math.sqrt(dx * dx + dy * dy + dz * dz) / speed * 20.0;
    }
}
