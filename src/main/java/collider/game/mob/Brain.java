package collider.game.mob;

import clojure.lang.IFn;
import clojure.lang.ILookup;
import clojure.lang.IPersistentVector;
import clojure.lang.ISeq;
import clojure.lang.Keyword;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import clojure.lang.Tuple;
import java.util.HashMap;
import java.util.Map;

/// The behaviours of a breed in the order they start and tick, and
/// the tick that forgets, senses, starts and ticks them.
/// Each behaviour starts while its activity is active and the
/// memories of `needs` are there and those of `shuns` are not.
/// Running behaviours tick whatever their activity. A part returns
/// the mob or `[mob deltas]`, a start nil when it declines.
public record Brain(
        Object info,
        int n,
        Object[] memories,
        Object[] activities,
        long[] needs,
        long[] shuns,
        IFn sense,
        IFn[] starts,
        IFn[] ticks)
        implements ILookup {

    private static final Keyword BRAIN = Keyword.intern("brain");
    private static final Keyword MEMORIES = Keyword.intern("memories");
    private static final Keyword RUNNING = Keyword.intern("running");
    private static final Keyword ACTIVITY = Keyword.intern("activity");
    private static final Keyword CORE = Keyword.intern("core");

    @Override
    public Object valAt(Object k) {
        return RT.get(info, k);
    }

    @Override
    public Object valAt(Object k, Object notFound) {
        return RT.get(info, k, notFound);
    }

    /// Returns the indexes of `names` in the order a
    /// `java.util.HashMap` iterates them after `computeIfAbsent` added
    /// them in turn. A later name of a shared bucket comes first.
    public static IPersistentVector hashOrder(Object names) {
        Map<String, Long> m = new HashMap<>();
        long i = 0;
        for (ISeq s = RT.seq(names); s != null; s = s.next()) {
            Long x = i++;
            m.computeIfAbsent((String) s.first(), k -> x);
        }
        IPersistentVector v = PersistentVector.EMPTY;
        for (Long x : m.values()) v = v.cons(x);
        return v;
    }

    private static Object brain(Object e) {
        return RT.get(e, BRAIN);
    }

    private static long until(Object m) {
        return RT.longCast(RT.nth(m, 1));
    }

    private static Object forgotten(Object e, long t) {
        Object b = brain(e);
        Object mems = RT.get(b, MEMORIES);
        Object kept = mems;
        for (ISeq s = RT.seq(mems); s != null; s = s.next()) {
            Map.Entry<?, ?> m = (Map.Entry<?, ?>) s.first();
            if (t > until(m.getValue())) kept = RT.dissoc(kept, m.getKey());
        }
        if (kept == mems) return e;
        return RT.assoc(e, BRAIN, RT.assoc(b, MEMORIES, kept));
    }

    private long mask(Object e, long t) {
        Object mems = RT.get(brain(e), MEMORIES);
        long m = 0;
        for (int k = 0; k < memories.length; k++) {
            Object v = RT.get(mems, memories[k]);
            if (v != null && t <= until(v)) m |= 1L << k;
        }
        return m;
    }

    private boolean ready(int i, Object e, long t) {
        long m = mask(e, t);
        return (m & needs[i]) == needs[i] && (m & shuns[i]) == 0;
    }

    private static boolean running(Object e, int i) {
        return RT.get(RT.get(brain(e), RUNNING), (long) i) != null;
    }

    private boolean active(Object e, int i) {
        Object a = activities[i];
        return a == CORE || a == RT.get(brain(e), ACTIVITY);
    }

    private static IPersistentVector into(IPersistentVector ds, Object r) {
        if (!(r instanceof IPersistentVector v)) return ds;
        for (ISeq s = RT.seq(v.nth(1)); s != null; s = s.next()) {
            ds = ds.cons(s.first());
        }
        return ds;
    }

    private static Object mob(Object r) {
        return r instanceof IPersistentVector v ? v.nth(0) : r;
    }

    private IPersistentVector started(
            Object w, Object eid, Object e, long t, IPersistentVector ds) {
        Object tt = t;
        for (int i = 0; i < n; i++) {
            if (!active(e, i) || running(e, i) || !ready(i, e, t)) continue;
            Object r = starts[i].invoke(w, eid, e, tt);
            if (r == null) continue;
            e = mob(r);
            ds = into(ds, r);
        }
        return Tuple.create(e, ds);
    }

    private IPersistentVector ticked(
            Object w, Object eid, Object e, long t, IPersistentVector ds) {
        int[] busy = new int[n];
        int m = 0;
        for (int i = 0; i < n; i++) {
            if (running(e, i)) busy[m++] = i;
        }
        Object tt = t;
        for (int k = 0; k < m; k++) {
            Object r = ticks[busy[k]].invoke(w, eid, e, tt);
            e = mob(r);
            ds = into(ds, r);
        }
        return Tuple.create(e, ds);
    }

    /// Returns `[e deltas]` after one brain tick of mob `e` with id
    /// `eid` in world `w` at tick `t`. Expired memories go first,
    /// the sensors run, the behaviours that may start start, and
    /// last every running behaviour ticks or stops.
    public static Object think(Brain b, Object w, Object eid, Object e, long t) {
        e = forgotten(e, t);
        IPersistentVector ds = PersistentVector.EMPTY;
        if (b.sense != null) {
            Object r = b.sense.invoke(w, eid, e, t);
            e = mob(r);
            ds = into(ds, r);
        }
        IPersistentVector r = b.started(w, eid, e, t, ds);
        return b.ticked(w, eid, r.nth(0), t, (IPersistentVector) r.nth(1));
    }
}
