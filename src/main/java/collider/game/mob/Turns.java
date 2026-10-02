package collider.game.mob;

import clojure.lang.IFn;
import java.util.concurrent.CountedCompleter;
import java.util.concurrent.atomic.AtomicIntegerArray;

/// The turns of the bodies of one island in parallel. Each body first
/// thinks, which needs no other body, then moves as soon as every
/// body of lower id that it could meet moved.
public final class Turns extends CountedCompleter<Void> {

    private static final int LEAF = 8;

    private final Turns root;
    private final PushGrid grid;
    private final double far;
    private final IFn mind, body;
    private final AtomicIntegerArray waiting;
    private final int[][] after;
    private final int lo, hi;
    private final boolean moving;

    private Turns(
            Turns root,
            PushGrid grid,
            double far,
            IFn mind,
            IFn body,
            AtomicIntegerArray waiting,
            int[][] after,
            int lo,
            int hi,
            boolean moving) {
        super(root);
        this.root = root == null ? this : root;
        this.grid = grid;
        this.far = far;
        this.mind = mind;
        this.body = body;
        this.waiting = waiting;
        this.after = after;
        this.lo = lo;
        this.hi = hi;
        this.moving = moving;
    }

    /// Calls `mind` and then `body` with the index of each body of the
    /// pinned grid `g`. The call of `body` with index s waits for the
    /// calls with each body of lower index that s could meet in a tick
    /// in which no body moves further than `reach` along x or z.
    public static void run(PushGrid g, double reach, IFn mind, IFn body) {
        int n = g.bodies();
        new Turns(null, g, 2.0 * reach + 1.0E-4, mind, body, new AtomicIntegerArray(n), new int[n][], 0, n, false)
                .invoke();
    }

    private void spawn(int lo, int hi, boolean moving) {
        root.addToPendingCount(1);
        new Turns(root, grid, far, mind, body, waiting, after, lo, hi, moving).fork();
    }

    private void thought(int s) {
        mind.invoke(s);
        int[] near = grid.near(s, far);
        after[s] = near;
        if (waiting.addAndGet(s, near[0]) == 0) spawn(s, s + 1, true);
    }

    private void moved(int first) {
        int s = first;
        while (s >= 0) {
            body.invoke(s);
            int next = -1;
            int[] ts = after[s];
            for (int k = 1; k < ts.length; k++) {
                int t = ts[k];
                if (waiting.decrementAndGet(t) == 0) {
                    if (next < 0) next = t;
                    else spawn(t, t + 1, true);
                }
            }
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
