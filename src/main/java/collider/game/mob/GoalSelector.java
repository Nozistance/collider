package collider.game.mob;

import clojure.lang.IFn;
import clojure.lang.ILookup;
import clojure.lang.IPersistentVector;
import clojure.lang.Keyword;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import clojure.lang.Tuple;

/// The goals of a breed, highest priority first, and the selector
/// that stops, starts and ticks them. Goals of one priority never
/// displace each other. A goal without a running function runs
/// while the task of the mob has its kind. A start that returns
/// `[e deltas false]` declines to start but keeps what it changed
/// on the mob.
public record GoalSelector(
        Object goals,
        Object childLook,
        int n,
        Object[] kinds,
        long[] prios,
        long[] masks,
        IFn[] runnings,
        IFn[] stops,
        IFn[] starts,
        IFn[] continues,
        IFn[] ticks,
        boolean[] every)
        implements ILookup {

    private static final Keyword GOALS = Keyword.intern("goals");
    private static final Keyword CHILD_LOOK = Keyword.intern("child-look");
    private static final Keyword TASK = Keyword.intern("task");
    private static final Keyword KIND = Keyword.intern("kind");

    public Object valAt(Object k) {
        return valAt(k, null);
    }

    public Object valAt(Object k, Object notFound) {
        if (k == GOALS) return goals;
        if (k == CHILD_LOOK) return childLook;
        return notFound;
    }

    private static Object kind(Object e) {
        return RT.get(RT.get(e, TASK), KIND);
    }

    private boolean running(int i, Object e, Object t, Object kind) {
        IFn f = runnings[i];
        return f != null ? RT.booleanCast(f.invoke(e, t)) : kinds[i] == kind;
    }

    private long bits(Object e, Object t) {
        Object kind = kind(e);
        long b = 0;
        for (int i = 0; i < n; i++) {
            if (running(i, e, t, kind)) b |= 1L << i;
        }
        return b;
    }

    private Object stopped(int i, Object e, Object t) {
        IFn f = stops[i];
        return f != null ? f.invoke(e, t) : RT.assoc(e, TASK, null);
    }

    private long[] locks(long b) {
        long[] held = {-1, -1, -1, -1};
        for (int i = 0; i < n; i++) {
            if ((b & (1L << i)) == 0) continue;
            for (int f = 0; f < 4; f++) {
                if ((masks[i] & (1L << f)) != 0) held[f] = prios[i];
            }
        }
        return held;
    }

    private static boolean free(long[] locked, long prio, long mask) {
        for (int f = 0; f < 4; f++) {
            long p = locked[f];
            if ((mask & (1L << f)) != 0 && p >= 0 && p <= prio) {
                return false;
            }
        }
        return true;
    }

    private static boolean held(long[] locked, long mask) {
        for (int f = 0; f < 4; f++) {
            if ((mask & (1L << f)) != 0 && locked[f] >= 0) return true;
        }
        return false;
    }

    private Object displaced(Object e, Object t, long mask) {
        for (int i = 0; i < n; i++) {
            if ((masks[i] & mask) > 0 && running(i, e, t, kind(e))) {
                e = stopped(i, e, t);
            }
        }
        return e;
    }

    private Object cleaned(Object w, Object e, Object t, Object ts) {
        Object kind = kind(e);
        for (int i = 0; i < n; i++) {
            if (running(i, e, t, kind) && !RT.booleanCast(continues[i].invoke(w, e, t, ts))) {
                e = stopped(i, e, t);
                kind = kind(e);
            }
        }
        return e;
    }

    private Object started(
            Object w,
            Object eid,
            Object e,
            Object t,
            Object ts,
            long[] locked,
            int i
    ) {
        Object r = starts[i].invoke(w, eid, e, t, ts);
        if (r == null || declined(r) || !held(locked, masks[i])) return r;
        return starts[i].invoke(w, eid, displaced(e, t, masks[i]), t, ts);
    }

    private static boolean declined(Object r) {
        return RT.count(r) > 2 && !RT.booleanCast(RT.nth(r, 2));
    }

    private static IPersistentVector into(IPersistentVector ds, Object more) {
        for (Object s = RT.seq(more); s != null; s = RT.next(s)) {
            ds = ds.cons(RT.first(s));
        }
        return ds;
    }

    private IPersistentVector selected(Object w, Object eid, Object e, Object t, Object ts) {
        IPersistentVector ds = PersistentVector.EMPTY;
        long b = bits(e, t);
        long[] locked = locks(b);
        for (int i = 0; i < n; i++) {
            boolean busy = (b & (1L << i)) != 0;
            if (busy || !free(locked, prios[i], masks[i])) continue;
            Object r = started(w, eid, e, t, ts, locked, i);
            if (r == null) continue;
            e = RT.nth(r, 0);
            if (declined(r)) continue;
            ds = into(ds, RT.nth(r, 1));
            b = bits(e, t);
            locked = locks(b);
        }
        return Tuple.create(e, ds);
    }

    private IPersistentVector ticked(
            Object w,
            Object eid,
            Object e,
            Object t,
            Object ts,
            IPersistentVector ds,
            boolean all
    ) {
        for (int i = 0; i < n; i++) {
            IFn f = ticks[i];
            if (f != null && (all || every[i]) && running(i, e, t, kind(e))) {
                Object r = f.invoke(this, w, eid, e, t, ts);
                e = RT.nth(r, 0);
                ds = into(ds, RT.nth(r, 1));
            }
        }
        return Tuple.create(e, ds);
    }

    /// Returns `[e deltas]` after one tick of the goals of mob `e`
    /// with id `eid` in world `w` at tick `t`. On a `full` tick the
    /// selector stops the goals that may not go on, starts those
    /// that may and ticks all. On other ticks it ticks only the goals
    /// that want every tick.
    public static Object think(
            GoalSelector s,
            Object w,
            Object eid,
            Object e,
            Object t,
            Object ts,
            boolean full
    ) {
        if (!full) {
            return s.ticked(w, eid, e, t, ts, PersistentVector.EMPTY, false);
        }
        IPersistentVector r = s.selected(w, eid, s.cleaned(w, e, t, ts), t, ts);
        return s.ticked(w, eid, r.nth(0), t, ts, (IPersistentVector) r.nth(1), true);
    }
}
