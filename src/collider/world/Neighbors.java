package collider.world;

import clojure.lang.Counted;
import clojure.lang.IFn;
import clojure.lang.Indexed;
import clojure.lang.ITransientCollection;
import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentVector;
import clojure.lang.RT;

import java.util.ArrayDeque;
import java.util.ArrayList;

/// A run of block updates over one window of edits: the chunks, what
/// the run collects, and the queue of neighbour updates. The queue
/// keeps the order of vanilla's `CollectingNeighborUpdater`: updates
/// added while one runs wait in a layer, and the layer goes on top of
/// the stack in the order it was added once the running one yields.
public final class Neighbors {

    private static final int[] DX = {-1, 1, 0, 0, 0, 0};
    private static final int[] DY = {0, 0, -1, 1, 0, 0};
    private static final int[] DZ = {0, 0, 0, 0, -1, 1};

    public Object chunks;
    private ITransientCollection records, writes, ticks, sent;
    private final ArrayList<Object> placed = new ArrayList<>();
    private final ArrayDeque<Object> stack = new ArrayDeque<>();
    private final ArrayList<Object> added = new ArrayList<>();
    private int count;

    public Neighbors(Object chunks) {
        this.chunks = chunks;
        records = PersistentVector.EMPTY.asTransient();
        writes = PersistentVector.EMPTY.asTransient();
        ticks = PersistentVector.EMPTY.asTransient();
        sent = PersistentVector.EMPTY.asTransient();
    }

    public Neighbors record(Object x) {
        records = records.conj(x);
        return this;
    }

    public Neighbors write(Object x) {
        writes = writes.conj(x);
        return this;
    }

    public Neighbors tick(Object x) {
        ticks = ticks.conj(x);
        return this;
    }

    public Neighbors send(Object x) {
        sent = sent.conj(x);
        return this;
    }

    public Neighbors addPlaced(Object x) {
        placed.add(x);
        return this;
    }

    public int writeCount() {
        return ((Counted) writes).count();
    }

    public Object writeAt(int n) {
        return ((Indexed) writes).nth(n);
    }

    /// Returns what the run collected as vectors: records, writes,
    /// ticks, sent and placed. The run ends here.
    public Object[] collected() {
        return new Object[] {records.persistent(), writes.persistent(),
                             ticks.persistent(), sent.persistent(),
                             PersistentVector.create(placed)};
    }

    /// Sets the block at `p`, which is `x` `y` `z`, to `st` with
    /// `flags` when its chunk is present, `y` is inside `minY` to
    /// `maxY` and the block is not `st` already; records the write and
    /// tells the clients when flag 2 asks. Returns the old state, or -1
    /// when nothing was set.
    public long place(Object p, int x, int y, int z, long st, long flags,
            int minY, int maxY) {
        if (y < minY || y > maxY) return -1;
        ChunkIndex c = (ChunkIndex) chunks;
        if (c.get(x >> 4, z >> 4) == null) return -1;
        long old = Chunk.blockAt(c, x, y, z);
        if (old == st) return -1;
        chunks = c.withBlock(x, y, z, (int) st);
        records = records.conj(vec(p, st));
        writes = writes.conj(vec(p, old, st, flags));
        if ((flags & 2) != 0) sent = sent.conj(p);
        return old;
    }

    /// Returns true when the six blocks beside `x` `y` `z` are all
    /// marked in `deaf`; outside the height counts as air.
    public boolean deafAround(long x, long y, long z, boolean[] deaf,
            int minY, int maxY) {
        ChunkIndex c = (ChunkIndex) chunks;
        for (int d = 0; d < 6; d++) {
            long ny = y + DY[d];
            int st = ny < minY || ny > maxY ? 0
                : Chunk.blockAt(c, (int) (x + DX[d]), (int) ny,
                                (int) (z + DZ[d]));
            if (!deaf[st]) return false;
        }
        return true;
    }

    /// Sets the changes of a command in order, each with `flags`. A
    /// change `[pos st]` of a state `plain` marks is set here, and
    /// when a block beside it is not `deaf`, `shaped` runs its shape
    /// updates as `(shaped run pos old)`; `other` sets any other change
    /// as `(other run change)`. Each change set here that alters its
    /// block joins the placed cells as `[pos old]`.
    public Neighbors command(Object changes, long flags, boolean[] plain,
            boolean[] deaf, IFn shaped, IFn other, int minY, int maxY) {
        for (Object c : (Iterable<?>) changes) {
            long st = RT.longCast(RT.nth(c, 1));
            Object fx = RT.nth(c, 2, null);
            if (!plain[(int) st] || RT.count(fx) != 0) {
                other.invoke(this, c);
                continue;
            }
            Object p = RT.nth(c, 0);
            int x = RT.intCast(RT.nth(p, 0)), y = RT.intCast(RT.nth(p, 1));
            int z = RT.intCast(RT.nth(p, 2));
            long old = place(p, x, y, z, st, flags, minY, maxY);
            if (old < 0) continue;
            placed.add(vec(p, old));
            if (!deafAround(x, y, z, deaf, minY, maxY)) {
                shaped.invoke(this, p, old);
            }
        }
        return this;
    }

    /// Runs `(told run pos old)` for each placed cell `[pos old]`
    /// with a block beside it that `deaf` does not mark.
    public Neighbors tell(boolean[] deaf, IFn told, int minY, int maxY) {
        for (Object c : placed) {
            Object p = RT.nth(c, 0);
            if (!deafAround(RT.longCast(RT.nth(p, 0)),
                            RT.longCast(RT.nth(p, 1)),
                            RT.longCast(RT.nth(p, 2)), deaf, minY, maxY)) {
                told.invoke(this, p, RT.nth(c, 1));
            }
        }
        return this;
    }

    /// Returns true while an update runs, so a new one waits.
    public boolean running() {
        return count > 0;
    }

    /// Adds `item`; when none runs, runs it and all it adds with
    /// `step`, a function of this run and an item that returns the
    /// item to run again or nil.
    public Neighbors addAndRun(Object item, IFn step) {
        if (count > 0) {
            added.add(item);
            return this;
        }
        count = 1;
        stack.push(item);
        for (;;) {
            for (int k = added.size() - 1; k >= 0; k--) {
                stack.push(added.get(k));
            }
            added.clear();
            if (stack.isEmpty()) break;
            for (;;) {
                Object next = step.invoke(this, stack.pop());
                if (next == null) break;
                stack.push(next);
                if (!added.isEmpty()) break;
            }
        }
        count = 0;
        return this;
    }

    private static Object vec(Object... xs) {
        return LazilyPersistentVector.createOwning(xs);
    }
}
