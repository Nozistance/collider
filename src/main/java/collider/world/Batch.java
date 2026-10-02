package collider.world;

import clojure.lang.ITransientCollection;
import clojure.lang.ITransientMap;
import clojure.lang.LazilyPersistentVector;
import clojure.lang.PersistentHashMap;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import java.util.Arrays;
import java.util.Objects;

/// The block edits of one section, applied at once in order.
public final class Batch {

    private int[] idx = new int[8];
    private int[] states = new int[8];
    private int n;

    /// Adds the edit that sets block `i` to `state`.
    public void add(int i, int state) {
        if (n == idx.length) {
            idx = Arrays.copyOf(idx, 2 * n);
            states = Arrays.copyOf(states, 2 * n);
        }
        idx[n] = i;
        states[n] = state;
        n++;
    }

    /// Returns `s` with the edits applied in order.
    public Section applyTo(Section s) {
        return s.apply(idx, states, n);
    }

    /// Returns the changes `[pos st]` that alter a block of `chunks`
    /// inside heights `minY` to `maxY`, as `[pos old st]` in order. A
    /// change sees the ones before it.
    public static Object changed(ChunkIndex chunks, Object changes, long minY, long maxY) {
        Scratch<Integer> now = new Scratch<>(RT.count(changes));
        ITransientCollection out = PersistentVector.EMPTY.asTransient();
        for (Object c : (Iterable<?>) changes) {
            Object p = RT.nth(c, 0);
            int x = RT.intCast(RT.nth(p, 0)), y = RT.intCast(RT.nth(p, 1));
            int z = RT.intCast(RT.nth(p, 2));
            int st = RT.intCast(RT.nth(c, 1));
            long k = ((long) (x & 0x3FFFFFF) << 38) | ((long) (z & 0x3FFFFFF) << 12) | (y & 0xFFF);
            Integer seen = now.get(k);
            int old = seen != null ? seen : Chunk.blockAt(chunks, x, y, z);
            if (old == st || y < minY || y > maxY) continue;
            out = out.conj(vec(p, (long) old, (long) st));
            now.put(k, st);
        }
        return out.persistent();
    }

    /// Returns `chunks` with the changes set, each `[pos st]`, or
    /// `[pos old st]` when `at` is 2. The edits of one section apply
    /// at once in order, the sections of a chunk from the top down so
    /// that a new section takes the sky light of the one above.
    /// Changes in absent chunks are dropped.
    public static ChunkIndex setBlocks(ChunkIndex chunks, Object changes, int at) {
        Scratch<Batch[]> m = new Scratch<>();
        for (Object c : (Iterable<?>) changes) {
            Object p = RT.nth(c, 0);
            int x = RT.intCast(RT.nth(p, 0)), y = RT.intCast(RT.nth(p, 1));
            int z = RT.intCast(RT.nth(p, 2));
            long id = ChunkIndex.id(x >> 4, z >> 4);
            Batch[] bs = m.get(id);
            if (bs == null) {
                boolean in = chunks.get(id) != null;
                bs = new Batch[in ? Chunk.COUNT : 0];
                m.put(id, bs);
            }
            if (bs.length == 0) continue;
            int si = (y >> 4) + 4;
            if (bs[si] == null) bs[si] = new Batch();
            bs[si].add(((y & 15) << 8) | ((z & 15) << 4) | (x & 15), RT.intCast(RT.nth(c, at)));
        }
        long[] ids = m.sortedKeys();
        Object[] vals = new Object[ids.length];
        int n = 0;
        for (long id : ids) {
            Batch[] bs = Objects.requireNonNull(m.get(id));
            if (bs.length == 0) continue;
            Chunk ch = (Chunk) chunks.get(id);
            for (int si = Chunk.COUNT - 1; si >= 0; si--) {
                if (bs[si] == null) continue;
                Section s = ch.section(si);
                if (s == null) s = ch.fresh(si);
                ch = ch.with(si, bs[si].applyTo(s));
            }
            ids[n] = id;
            vals[n++] = ch;
        }
        return chunks.withAll(Arrays.copyOf(ids, n), Arrays.copyOf(vals, n));
    }

    /// Returns the changes `[pos old st]` as a map from chunk id to
    /// the `[pos st]` of that chunk, in order.
    public static Object byChunk(Object changes) {
        Scratch<ITransientCollection> m = new Scratch<>();
        for (Object c : (Iterable<?>) changes) {
            Object p = RT.nth(c, 0);
            long id = ChunkIndex.id(RT.intCast(RT.nth(p, 0)) >> 4, RT.intCast(RT.nth(p, 2)) >> 4);
            ITransientCollection v = m.get(id);
            if (v == null) v = PersistentVector.EMPTY.asTransient();
            m.put(id, v.conj(vec(p, RT.nth(c, 2))));
        }
        ITransientMap r = PersistentHashMap.EMPTY.asTransient();
        for (long id : m.sortedKeys())
            r = r.assoc(id, Objects.requireNonNull(m.get(id)).persistent());
        return r.persistent();
    }

    private static Object vec(Object... xs) {
        return LazilyPersistentVector.createOwning(xs);
    }
}
