package collider.game.mob;

import clojure.lang.IFn;
import java.util.concurrent.CountedCompleter;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicIntegerArray;

/// The turns of the bodies of one island in parallel. Each body first
/// thinks, which needs no other body, then moves as soon as every
/// body of lower id that it could meet moved. The turns join in the
/// order of the bodies while later bodies still move.
public final class Turns extends CountedCompleter<Void> {

    private static final int LEAF = 8;

    private final Turns root;
    private final PushGrid grid;
    private final double far;
    private final IFn mind, body, join;
    private final AtomicIntegerArray waiting, moved;
    private final AtomicInteger joined;
    private final AtomicBoolean joining;
    private final int[][] after;
    private final int lo, hi;
    private final boolean moving;

    private Turns(Turns root, int lo, int hi, boolean moving) {
        super(root);
        this.root = root;
        this.grid = root.grid;
        this.far = root.far;
        this.mind = root.mind;
        this.body = root.body;
        this.join = root.join;
        this.waiting = root.waiting;
        this.moved = root.moved;
        this.joined = root.joined;
        this.joining = root.joining;
        this.after = root.after;
        this.lo = lo;
        this.hi = hi;
        this.moving = moving;
    }

    private Turns(PushGrid grid, double far, IFn mind, IFn body, IFn join) {
        super(null);
        int n = grid.bodies();
        this.root = this;
        this.grid = grid;
        this.far = far;
        this.mind = mind;
        this.body = body;
        this.join = join;
        this.waiting = new AtomicIntegerArray(n);
        this.moved = new AtomicIntegerArray(n);
        this.joined = new AtomicInteger();
        this.joining = new AtomicBoolean();
        this.after = new int[n][];
        this.lo = 0;
        this.hi = n;
        this.moving = false;
    }

    /// Calls `mind` and then `body` with the index of each body of the
    /// pinned grid `g`, and `join` with each index in ascending order.
    /// The call of `body` with index s waits for the calls with each
    /// body of lower index that s could meet in a tick in which no
    /// body moves further than `reach` along x or z. The call of
    /// `join` with s follows the calls of `body` with s and each lower
    /// index, and no two calls of `join` overlap.
    public static void run(PushGrid g, double reach, IFn mind, IFn body, IFn join) {
        new Turns(g, 2.0 * reach + 1.0E-4, mind, body, join).invoke();
    }

    private void spawn(int lo, int hi, boolean moving) {
        root.addToPendingCount(1);
        new Turns(root, lo, hi, moving).fork();
    }

    private void thought(int s) {
        mind.invoke(s);
        int[] near = grid.near(s, far);
        after[s] = near;
        if (waiting.addAndGet(s, near[0]) == 0) spawn(s, s + 1, true);
    }

    private boolean ready(int s) {
        return s < moved.length() && moved.get(s) != 0;
    }

    private void joinReady() {
        while (ready(joined.get()) && joining.compareAndSet(false, true)) {
            int s = joined.get();
            while (ready(s)) join.invoke(s++);
            joined.set(s);
            joining.set(false);
        }
    }

    private void moved(int first) {
        int s = first;
        while (s >= 0) {
            body.invoke(s);
            moved.set(s, 1);
            int next = -1;
            int[] ts = after[s];
            for (int k = 1; k < ts.length; k++) {
                int t = ts[k];
                if (waiting.decrementAndGet(t) == 0) {
                    if (next < 0) next = t;
                    else spawn(t, t + 1, true);
                }
            }
            joinReady();
            s = next;
        }
    }

    @Override
    public void compute() {
        if (root == this) {
            for (int s = lo; s < hi; s += LEAF) {
                spawn(s, Math.min(hi, s + LEAF), false);
            }
        } else if (moving) {
            moved(lo);
        } else {
            for (int s = lo; s < hi; s++) thought(s);
        }
        tryComplete();
    }
}
