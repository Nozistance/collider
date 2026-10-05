package collider.world;

import clojure.lang.Counted;
import clojure.lang.IFn;
import clojure.lang.ITransientCollection;
import clojure.lang.Indexed;
import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import clojure.lang.Util;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;

/// A run of block updates over one window of edits, with the chunks,
/// what the run collects and the queue of neighbour updates. Updates
/// added while one runs wait in a layer, and the layer goes on top of
/// the stack in the order it was added once the running one yields.
/// In Java because it runs a mutable queue of updates over primitive
/// arrays.
public final class Neighbors {

    private static final long CLIENTS = 2;

    /// The x offsets of the six sides in the order of updates, which is
    /// west, east, down, up, north, south. Shape updates share the x
    /// offsets and go west, east, north, south, down, up.
    private static final int[] DX = {-1, 1, 0, 0, 0, 0};
    private static final int[] DY = {0, 0, -1, 1, 0, 0};
    private static final int[] DZ = {0, 0, 0, 0, -1, 1};
    private static final int[] SY = {0, 0, 0, 0, -1, 1};
    private static final int[] SZ = {0, 0, -1, 1, 0, 0};

    /// The chunks of the window as the run left them so far.
    public ChunkIndex chunks;
    private ITransientCollection records, writes, ticks, sent;
    private final ArrayList<Object> placed = new ArrayList<>();
    private long[] placedOld = new long[16];
    private final ArrayDeque<Object> stack = new ArrayDeque<>();
    private final ArrayList<Object> added = new ArrayList<>();
    private boolean running;

    /// Starts a run over the window `chunks`.
    public Neighbors(ChunkIndex chunks) {
        this.chunks = chunks;
    }

    private static ITransientCollection conj(ITransientCollection v, Object x) {
        if (v == null) v = PersistentVector.EMPTY.asTransient();
        return v.conj(x);
    }

    private static Object done(ITransientCollection v) {
        return v == null ? PersistentVector.EMPTY : v.persistent();
    }

    public Neighbors record(Object x) {
        records = conj(records, x);
        return this;
    }

    public Neighbors write(Object x) {
        writes = conj(writes, x);
        return this;
    }

    public Neighbors tick(Object x) {
        ticks = conj(ticks, x);
        return this;
    }

    /// Adds `x` to what the clients are told.
    public Neighbors send(Object x) {
        sent = conj(sent, x);
        return this;
    }

    /// Adds `p` to the placed cells, `old` being its state before.
    public Neighbors addPlaced(Object p, long old) {
        int n = placed.size();
        if (n == placedOld.length) placedOld = Arrays.copyOf(placedOld, 2 * n);
        placedOld[n] = old;
        placed.add(p);
        return this;
    }

    public int recordCount() {
        return records == null ? 0 : ((Counted) records).count();
    }

    public int writeCount() {
        return writes == null ? 0 : ((Counted) writes).count();
    }

    public Object writeAt(int n) {
        return ((Indexed) writes).nth(n);
    }

    /// Returns the records, writes, ticks, sent cells and placed cells
    /// that the run collected. The run ends here.
    public Object[] collected() {
        return new Object[] {
            done(records),
            done(writes),
            done(ticks),
            done(sent),
            PersistentVector.create(placed)
        };
    }

    /// Returns the distinct cells of the `records` of a run that its
    /// `sent` cells do not hold, in order.
    public static Object unheard(Object records, Object sent) {
        if (paired(records, sent)) return PersistentVector.EMPTY;
        Scratch<Boolean> heard = new Scratch<>(RT.count(sent));
        for (Object p : (Iterable<?>) sent) heard.put(packed(p), Boolean.TRUE);
        Scratch<Boolean> seen = new Scratch<>();
        ITransientCollection out = PersistentVector.EMPTY.asTransient();
        for (Object r : (Iterable<?>) records) {
            Object p = RT.nth(r, 0);
            long k = packed(p);
            if (heard.get(k) != null || seen.get(k) != null) continue;
            seen.put(k, Boolean.TRUE);
            out = out.conj(p);
        }
        return out.persistent();
    }

    /// Returns true when each record has its own sent cell, in order.
    private static boolean paired(Object records, Object sent) {
        Indexed s = (Indexed) sent;
        int m = RT.count(sent), j = 0;
        for (Object r : (Iterable<?>) records) {
            if (j == m || !Util.equiv(RT.nth(r, 0), s.nth(j))) return false;
            j++;
        }
        return true;
    }

    private static long packed(Object p) {
        return Scratch.cell(
                RT.longCast(RT.nth(p, 0)),
                RT.longCast(RT.nth(p, 1)),
                RT.longCast(RT.nth(p, 2))
        );
    }

    /// Sets the block at `p`, which is `x` `y` `z`, to `st` with
    /// `flags` when its chunk is present, `y` is inside `minY` to
    /// `maxY` and the block is not `st` already. Records the write and
    /// tells the clients when the flags ask. Returns the old state, or
    /// -1 when nothing was set.
    public long place(
            Object p,
            int x,
            int y,
            int z,
            long st,
            long flags,
            int minY,
            int maxY
    ) {
        return put(p, null, x, y, z, st, flags, minY, maxY);
    }

    /// Places as `place` does, recording `rec` when it is the change
    /// `[p st]` itself, else a new one.
    private long put(
            Object p,
            Object rec,
            int x,
            int y,
            int z,
            long st,
            long flags,
            int minY,
            int maxY
    ) {
        if (y < minY || y > maxY) return -1;
        ChunkIndex c = chunks;
        if (c.get(x >> 4, z >> 4) == null) return -1;
        long old = Chunk.blockAt(c, x, y, z);
        if (old == st) return -1;
        chunks = c.withBlock(x, y, z, (int) st);
        Object boxed = rec != null ? RT.nth(rec, 1) : Long.valueOf(st);
        records = conj(records, rec != null ? rec : vec(p, boxed));
        writes = conj(writes, vec(p, old, boxed, flags));
        if ((flags & CLIENTS) != 0) sent = conj(sent, p);
        return old;
    }

    /// Returns true when the six blocks beside `x` `y` `z` are all
    /// marked in `deaf`. Outside the height counts as air.
    public boolean deafAround(
            long x,
            long y,
            long z,
            boolean[] deaf,
            int minY,
            int maxY
    ) {
        ChunkIndex c = chunks;
        for (int d = 0; d < 6; d++) {
            int st = stateAt(c, x + DX[d], y + DY[d], z + DZ[d], minY, maxY);
            if (!deaf[st]) return false;
        }
        return true;
    }

    private static int stateAt(
            ChunkIndex c,
            long x,
            long y,
            long z,
            int minY,
            int maxY
    ) {
        if (y < minY || y > maxY) return 0;
        return Chunk.blockAt(c, (int) x, (int) y, (int) z);
    }

    /// Sets the changes of a command in order, each with `flags`. A
    /// change `[pos st]` of a state `plain` marks is set here, and when a
    /// block beside it is not `deaf`, `shaped` runs its shape updates as
    /// `(shaped run pos old)`. `other` sets any other change as
    /// `(other run change)`. Each change set here that alters its block
    /// joins the placed cells.
    public Neighbors command(
            Object changes,
            long flags,
            boolean[] plain,
            boolean[] deaf,
            IFn shaped,
            IFn other,
            int minY,
            int maxY
    ) {
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
            Object rec = RT.count(c) == 2 && RT.nth(c, 1) instanceof Long ? c : null;
            long old = put(p, rec, x, y, z, st, flags, minY, maxY);
            if (old < 0) continue;
            addPlaced(p, old);
            if (!deafAround(x, y, z, deaf, minY, maxY)) {
                shaped.invoke(this, p, old);
            }
        }
        return this;
    }

    /// Runs `(told run pos old)` for each placed cell `pos`, `old`
    /// being its state before, with a block beside it that `deaf` does
    /// not mark.
    public Neighbors tell(boolean[] deaf, IFn told, int minY, int maxY) {
        for (int i = 0; i < placed.size(); i++) {
            Object p = placed.get(i);
            if (!deafAround(
                    RT.longCast(RT.nth(p, 0)),
                    RT.longCast(RT.nth(p, 1)),
                    RT.longCast(RT.nth(p, 2)),
                    deaf,
                    minY,
                    maxY
            )) {
                told.invoke(this, p, placedOld[i]);
            }
        }
        return this;
    }

    /// Returns true while an update runs, so a new one waits.
    public boolean running() {
        return running;
    }

    /// Adds `item`. When none runs, it runs the item and all it adds with
    /// `step`, a function of this run and an item that returns the item to
    /// run again or nil.
    public Neighbors addAndRun(Object item, IFn step) {
        if (running) {
            added.add(item);
            return this;
        }
        running = true;
        stack.push(item);
        for (; ; ) {
            for (int k = added.size() - 1; k >= 0; k--) {
                stack.push(added.get(k));
            }
            added.clear();
            if (stack.isEmpty()) break;
            for (; ; ) {
                Object top = stack.pop();
                Object next = top instanceof Pass p
                        ? p.step(this)
                        : step.invoke(this, top);
                if (next == null) break;
                stack.push(next);
                if (!added.isEmpty()) break;
            }
        }
        running = false;
        return this;
    }

    /// Adds a pass over the six blocks beside `x` `y` `z` and runs it
    /// like `addAndRun`. The pass goes in the order of shape updates
    /// when `shape` is true, else of neighbour updates, and runs
    /// `(told run pos side st)` for each block that `deaf` does not
    /// mark when it comes to it, `st` being its state then. `sides`
    /// names the side of each step as seen from the block told.
    /// Outside the height counts as air.
    public Neighbors pass(
            long x,
            long y,
            long z,
            boolean shape,
            boolean[] deaf,
            Object[] sides,
            IFn told,
            IFn step,
            int minY,
            int maxY
    ) {
        int[] dy = shape ? SY : DY;
        int[] dz = shape ? SZ : DZ;
        Pass p = new Pass(x, y, z, dy, dz, deaf, sides, told, minY, maxY);
        return addAndRun(p, step);
    }

    private static final class Pass {
        private final long x, y, z;
        private final int[] dy, dz;
        private final boolean[] deaf;
        private final Object[] sides;
        private final IFn told;
        private final int minY, maxY;
        private int i;

        Pass(
                long x,
                long y,
                long z,
                int[] dy,
                int[] dz,
                boolean[] deaf,
                Object[] sides,
                IFn told,
                int minY,
                int maxY
        ) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.dy = dy;
            this.dz = dz;
            this.deaf = deaf;
            this.sides = sides;
            this.told = told;
            this.minY = minY;
            this.maxY = maxY;
        }

        Object step(Neighbors s) {
            while (i < 6) {
                int d = i++;
                long nx = x + DX[d], ny = y + dy[d], nz = z + dz[d];
                int st = stateAt(s.chunks, nx, ny, nz, minY, maxY);
                if (deaf[st]) continue;
                told.invoke(s, vec(nx, ny, nz), sides[d], (long) st);
                break;
            }
            return i < 6 ? this : null;
        }
    }

    private static Object vec(Object... xs) {
        return LazilyPersistentVector.createOwning(xs);
    }
}
