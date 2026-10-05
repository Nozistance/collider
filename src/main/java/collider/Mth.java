package collider;

/// The angle arithmetic of the game, rounded as the game rounds it.
/// In Java because it keeps the float arithmetic of the game.
public final class Mth {

    private static final double FRAC_BIAS = Double.longBitsToDouble(4805340802404319232L);
    private static final double[] ASIN_TAB = new double[257];
    private static final double[] COS_TAB = new double[257];

    static {
        for (int i = 0; i < 257; i++) {
            double a = Math.asin((double) i / 256.0);
            COS_TAB[i] = Math.cos(a);
            ASIN_TAB[i] = a;
        }
    }

    /// Returns the angle of `y`, `x` in radians as the game finds it,
    /// close to the exact arctangent but not equal.
    @SuppressWarnings("SuspiciousNameCombination")
    public static double atan2(double y, double x) {
        double d2 = x * x + y * y;
        if (Double.isNaN(d2)) return Double.NaN;
        boolean negY = y < 0.0;
        if (negY) y = -y;
        boolean negX = x < 0.0;
        if (negX) x = -x;
        boolean steep = y > x;
        if (steep) {
            double t = x;
            x = y;
            y = t;
        }
        double rinv = invSqrt(d2);
        x *= rinv;
        y *= rinv;
        double yp = FRAC_BIAS + y;
        int index = (int) Double.doubleToRawLongBits(yp);
        double phi = ASIN_TAB[index];
        double cPhi = COS_TAB[index];
        double sPhi = yp - FRAC_BIAS;
        double sd = y * cPhi - x * sPhi;
        double d = (6.0 + sd * sd) * sd * 0.16666666666666666;
        double theta = phi + d;
        if (steep) theta = (Math.PI / 2) - theta;
        if (negX) theta = Math.PI - theta;
        if (negY) theta = -theta;
        return theta;
    }

    private static double invSqrt(double x) {
        double xhalf = 0.5 * x;
        long i = Double.doubleToRawLongBits(x);
        i = 6910469410427058090L - (i >> 1);
        x = Double.longBitsToDouble(i);
        return x * (1.5 - xhalf * x * x);
    }

    /// Returns the angle `a` in degrees brought into -180 to 180, in
    /// floats.
    public static float wrapDegrees(float a) {
        float n = a % 360.0F;
        if (n >= 180.0F) n -= 360.0F;
        if (n < -180.0F) n += 360.0F;
        return n;
    }
}
