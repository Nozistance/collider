package collider.game.mob;

import clojure.lang.ILookup;
import clojure.lang.Keyword;
import clojure.lang.RT;

/// The move control of a mob: what it does, where it walks to, the
/// speed modifier it was given and the speed and forward input it
/// drives with. A value; each change makes a new one.
public record Steer(Keyword op, double x, double y, double z, double mult, double speed, double zza)
        implements ILookup {

    public static final Keyword MOVE_TO = Keyword.intern("move-to");

    public static final Keyword JUMPING = Keyword.intern("jumping");

    public static final Keyword WAIT = Keyword.intern("wait");

    private static final Keyword OP = Keyword.intern("op");
    private static final Keyword X = Keyword.intern("x");
    private static final Keyword Y = Keyword.intern("y");
    private static final Keyword Z = Keyword.intern("z");
    private static final Keyword MULT = Keyword.intern("mult");
    private static final Keyword SPEED = Keyword.intern("speed");
    private static final Keyword ZZA = Keyword.intern("zza");

    private static final double DEGREES = 180.0F / (float) Math.PI;

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

    /// Returns the angle of `y`, `x` as Mth.atan2 approximates it.
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

    private static float wrapDegrees(float a) {
        float n = a % 360.0F;
        if (n >= 180.0F) n -= 360.0F;
        if (n < -180.0F) n += 360.0F;
        return n;
    }

    /// Returns yaw turned toward the offset `xd`, `zd` by at most
    /// `max` degrees, in floats as MoveControl.tick and rotlerp.
    public static double turned(double yaw, double xd, double zd, double max) {
        float to = (float) (atan2(zd, xd) * DEGREES) - 90.0F;
        return rotlerp(yaw, to, max);
    }

    /// Returns `a0` turned toward `b0` by at most `max` degrees, brought
    /// back into one turn, in floats as MoveControl.rotlerp.
    public static double rotlerp(double a0, double b0, double max) {
        float a = (float) a0, b = (float) b0, m = (float) max;
        float diff = wrapDegrees(b - a);
        if (diff > m) diff = m;
        if (diff < -m) diff = -m;
        float r = a + diff;
        if (r < 0.0F) {
            r += 360.0F;
        } else if (r > 360.0F) {
            r -= 360.0F;
        }
        return r;
    }

    /// The control of a mob never told to move.
    public static final Steer IDLE = new Steer(WAIT, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

    private static double num(Object m, Keyword k) {
        Object v = RT.get(m, k);
        return v == null ? 0.0 : ((Number) v).doubleValue();
    }

    /// Returns the control `m` as a Steer, from a map when it is
    /// one. Keys left out read as waiting and zero.
    public static Steer of(Object m) {
        if (m instanceof Steer s) return s;
        if (m == null) return IDLE;
        Object op = RT.get(m, OP);
        return new Steer(
                op == null ? WAIT : (Keyword) op,
                num(m, X),
                num(m, Y),
                num(m, Z),
                num(m, MULT),
                num(m, SPEED),
                num(m, ZZA));
    }

    private static boolean same(double a, double b) {
        return Double.doubleToRawLongBits(a) == Double.doubleToRawLongBits(b);
    }

    /// Returns `s` itself when it already holds these values bit for
    /// bit, else a new control with them.
    private static Steer kept(
            Steer s,
            Keyword op,
            double x,
            double y,
            double z,
            double mult,
            double speed,
            double zza
    ) {
        if (s.op == op
                && same(s.x, x)
                && same(s.y, y)
                && same(s.z, z)
                && same(s.mult, mult)
                && same(s.speed, speed)
                && same(s.zza, zza)) {
            return s;
        }
        return new Steer(op, x, y, z, mult, speed, zza);
    }

    /// Returns `m` told to walk to `x`, `y`, `z` with speed modifier
    /// `mult`. A jump under way goes on.
    public static Steer wanted(Object m, double x, double y, double z, double mult) {
        Steer s = of(m);
        Keyword op = s.op == JUMPING ? JUMPING : MOVE_TO;
        return kept(s, op, x, y, z, mult, s.speed, s.zza);
    }

    /// Returns `m` arrived: waiting, with no forward input.
    public static Steer arrived(Object m) {
        Steer s = of(m);
        return kept(s, WAIT, s.x, s.y, s.z, s.mult, s.speed, 0.0);
    }

    /// Returns `m` driving at `speed`, rounded to a float, then
    /// jumping or waiting.
    public static Steer driven(Object m, double speed, boolean jump) {
        Steer s = of(m);
        double f = (float) speed;
        return kept(s, jump ? JUMPING : WAIT, s.x, s.y, s.z, s.mult, f, f);
    }

    /// Returns `m` in its jump at `speed`, rounded to a float, which
    /// ends on landing.
    public static Steer jumped(Object m, double speed, boolean landed) {
        Steer s = of(m);
        double f = (float) speed;
        return kept(s, landed ? WAIT : s.op, s.x, s.y, s.z, s.mult, f, f);
    }

    /// Returns `m` with no forward input, the same `m` when it has
    /// none, and nil for no control.
    public static Object halted(Object m) {
        if (m == null) return null;
        Steer s = of(m);
        if (s.zza == 0.0 && m instanceof Steer) return m;
        return kept(s, s.op, s.x, s.y, s.z, s.mult, s.speed, 0.0);
    }

    public Object valAt(Object k) {
        return valAt(k, null);
    }

    public Object valAt(Object k, Object notFound) {
        if (k == OP) return op;
        if (k == X) return x;
        if (k == Y) return y;
        if (k == Z) return z;
        if (k == MULT) return mult;
        if (k == SPEED) return speed;
        if (k == ZZA) return zza;
        return notFound;
    }
}
