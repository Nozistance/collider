package collider.game.mob;

import clojure.lang.ILookup;
import clojure.lang.Keyword;
import clojure.lang.RT;

/// The move control of a mob: what it does, where it walks to, the
/// speed modifier it was given and the speed and forward input it
/// drives with. A value; each change makes a new one.
public record Steer(Keyword op, double x, double y, double z,
        double mult, double speed, double zza) implements ILookup {

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

    /// The control of a mob never told to move.
    public static final Steer IDLE =
        new Steer(WAIT, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0);

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
        return new Steer(op == null ? WAIT : (Keyword) op, num(m, X),
                         num(m, Y), num(m, Z), num(m, MULT),
                         num(m, SPEED), num(m, ZZA));
    }

    /// Returns `m` told to walk to `x`, `y`, `z` with speed modifier
    /// `mult`. A jump under way goes on.
    public static Steer wanted(Object m, double x, double y, double z,
            double mult) {
        Steer s = of(m);
        Keyword op = s.op == JUMPING ? JUMPING : MOVE_TO;
        return new Steer(op, x, y, z, mult, s.speed, s.zza);
    }

    /// Returns `m` arrived: waiting, with no forward input.
    public static Steer arrived(Object m) {
        Steer s = of(m);
        return new Steer(WAIT, s.x, s.y, s.z, s.mult, s.speed, 0.0);
    }

    /// Returns `m` driving at `speed`, rounded to a float, then
    /// jumping or waiting.
    public static Steer driven(Object m, double speed, boolean jump) {
        Steer s = of(m);
        double f = (double) (float) speed;
        return new Steer(jump ? JUMPING : WAIT, s.x, s.y, s.z, s.mult,
                         f, f);
    }

    /// Returns `m` in its jump at `speed`, rounded to a float, which
    /// ends on landing.
    public static Steer jumped(Object m, double speed, boolean landed) {
        Steer s = of(m);
        double f = (double) (float) speed;
        return new Steer(landed ? WAIT : s.op, s.x, s.y, s.z, s.mult, f,
                         f);
    }

    /// Returns `m` with no forward input, the same `m` when it has
    /// none, and nil for no control.
    public static Object halted(Object m) {
        if (m == null) return null;
        Steer s = of(m);
        if (s.zza == 0.0 && m instanceof Steer) return m;
        return new Steer(s.op, s.x, s.y, s.z, s.mult, s.speed, 0.0);
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
