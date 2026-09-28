package collider.game.mob;

import clojure.lang.ITransientCollection;
import clojure.lang.PersistentVector;
import clojure.lang.RT;

/// The shoves between overlapping bodies in the cells of the
/// push grid.
public final class Push {

    private static final double STRENGTH = (double) 0.05F;

    private static final double THRESHOLD = (double) 0.01F;

    /// Returns the shoves between a body and each body of the `cs`
    /// cells that it overlaps and whose id is below `hi`, in the
    /// order found. A cell may be null. The body with id `eid`, half
    /// width `half` and height `height` stands at `x`, `y`, `z`. Each
    /// shove `[id dx dz]` moves this body by dx dz and the other body
    /// the opposite way. The shoves are not summed.
    public static Object shoves(Object[] cs, double x, double y,
            double z, double half, double height, long eid, long hi) {
        ITransientCollection acc = PersistentVector.EMPTY.asTransient();
        for (Object o : cs) {
            if (o != null) {
                cell(acc, (PushCell) o, x, y, z, half, height, eid, hi);
            }
        }
        return acc.persistent();
    }

    private static void cell(ITransientCollection acc, PushCell c,
            double x, double y, double z, double half, double height,
            long eid, long hi) {
        long[] ids = c.eids();
        double[] xs = c.xs(), ys = c.ys(), zs = c.zs();
        double[] halfs = c.halfs(), heights = c.heights();
        for (int j = 0; j < ids.length; j++) {
            long o = ids[j];
            if (o == eid || o >= hi) continue;
            double ox = xs[j], oy = ys[j], oz = zs[j];
            double r = half + halfs[j];
            if (Math.abs(ox - x) < r && Math.abs(oz - z) < r
                    && oy < y + height && oy + heights[j] > y) {
                shove(acc, o, x - ox, z - oz);
            }
        }
    }

    private static void shove(ITransientCollection acc, long o,
            double dx, double dz) {
        double m = Math.max(Math.abs(dx), Math.abs(dz));
        if (m >= THRESHOLD) {
            double s = Math.sqrt(m);
            double p = Math.min(1.0, 1.0 / s);
            acc.conj(RT.vector(o, dx / s * p * STRENGTH,
                               dz / s * p * STRENGTH));
        }
    }
}
