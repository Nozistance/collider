package collider.java;

/// Collision of a moving box against block boxes, one axis at a
/// time.
public final class Phys {

    private static final double EPS = 1.0E-7;

    /// Returns `d` clamped so the box `ebox` moving by `d` along
    /// `axis` stops on the face of the nearest of the `n` boxes in
    /// `a`, each stored as 6 consecutive doubles (min x, y, z, max
    /// x, y, z).
    public static double clampAll(double[] a, int n, double[] ebox,
            int eo, int axis, double d) {
        int p1 = (axis == 0) ? 1 : 0;
        int q1 = (axis == 2) ? 1 : 2;
        int p2 = p1 + 3, q2 = q1 + 3;
        double ep1 = ebox[eo + p1], ep2 = ebox[eo + p2];
        double eq1 = ebox[eo + q1], eq2 = ebox[eo + q2];
        double elo = ebox[eo + axis], ehi = ebox[eo + axis + 3];
        for (int i = 0; i < n; i++) {
            if (Math.abs(d) < EPS) return 0.0;
            int o = i * 6;
            if (ep1 + EPS < a[o + p2] && ep2 - EPS >= a[o + p1]
                && eq1 + EPS < a[o + q2]
                && eq2 - EPS >= a[o + q1]) {
                double blo = a[o + axis], bhi = a[o + axis + 3];
                if (d > 0.0 && ehi - EPS < blo) {
                    double m = blo - ehi;
                    if (m < d) d = m;
                } else if (d < 0.0 && elo + EPS >= bhi) {
                    double m = bhi - elo;
                    if (m > d) d = m;
                }
            }
        }
        return d;
    }

    /// Writes into `out` the clamped x, y, z motion of `box` through
    /// the velocity `vx`, `vy`, `vz` against the `n` boxes in `a`,
    /// resolving y first and then the wider of x and z last.
    public static void clampAxes(double[] a, int n, double[] box,
            double vx, double vy, double vz, double[] out) {
        double b0 = box[0], b1 = box[1], b2 = box[2];
        double b3 = box[3], b4 = box[4], b5 = box[5];
        double[] e = {b0, b1, b2, b3, b4, b5};
        double dy = clampAll(a, n, e, 0, 1, vy);
        e[1] = b1 + dy;
        e[4] = b4 + dy;
        double dx, dz;
        if (Math.abs(vx) < Math.abs(vz)) {
            dz = clampAll(a, n, e, 0, 2, vz);
            e[2] = b2 + dz;
            e[5] = b5 + dz;
            dx = clampAll(a, n, e, 0, 0, vx);
        } else {
            dx = clampAll(a, n, e, 0, 0, vx);
            e[0] = b0 + dx;
            e[3] = b3 + dx;
            dz = clampAll(a, n, e, 0, 2, vz);
        }
        out[0] = dx;
        out[1] = dy;
        out[2] = dz;
    }
}
