package collider.game.mob;

import clojure.lang.ILookup;
import clojure.lang.Keyword;
import clojure.lang.RT;
import collider.Mth;

/// The move control of a mob. It holds what the mob does, where it
/// walks to, the speed modifier it was given and the speed and
/// forward input it drives with. Each change makes a new control.
public record Steer(
        Keyword op,
        double x,
        double y,
        double z,
        double mult,
        double speed,
        double zza
) implements ILookup {

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

    /// Returns yaw turned toward the offset `xd`, `zd` by at most
    /// `max` degrees, in floats.
    public static double turned(double yaw, double xd, double zd, double max) {
        float to = (float) (Mth.atan2(zd, xd) * DEGREES) - 90.0F;
        return rotlerp(yaw, to, max);
    }

    /// Returns `a0` turned toward `b0` by at most `max` degrees, brought
    /// back into one turn, in floats.
    public static double rotlerp(double a0, double b0, double max) {
        float a = (float) a0, b = (float) b0, m = (float) max;
        float diff = Mth.wrapDegrees(b - a);
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

    /// Returns `m` arrived, waiting with no forward input.
    public static Steer arrived(Object m) {
        Steer s = of(m);
        return kept(s, WAIT, s.x, s.y, s.z, s.mult, s.speed, 0.0);
    }

    /// Returns `m` driving at `speed`, rounded to a float. It jumps
    /// when `jump` is true and waits otherwise.
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
