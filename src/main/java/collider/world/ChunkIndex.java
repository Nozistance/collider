package collider.world;

import clojure.lang.AFn;
import clojure.lang.APersistentMap;
import clojure.lang.ArraySeq;
import clojure.lang.IDeref;
import clojure.lang.IEditableCollection;
import clojure.lang.IFn;
import clojure.lang.IKVReduce;
import clojure.lang.IMapEntry;
import clojure.lang.IObj;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentVector;
import clojure.lang.IReduceInit;
import clojure.lang.ISeq;
import clojure.lang.ITransientAssociative2;
import clojure.lang.ITransientMap;
import clojure.lang.MapEntry;
import clojure.lang.RT;
import clojure.lang.Util;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;

/// A persistent map from chunk coordinates to chunk values.
///
/// An index may carry an owner `token`, opened by `editable()` and
/// closed by `frozen()`. Every edit of such an index reuses its
/// token, and `Edit.own` writes in place into nodes tagged with it.
/// A node tagged with token T is reachable only from indexes carrying
/// T, the ones edits of that window returned. A frozen index has no
/// token, an edit of it makes a fresh one, and no later window reuses
/// T, so nodes tagged T are never written again and frozen indexes
/// are immutable values. Inside a window only the index returned last
/// is valid to read, as with a transient.
public final class ChunkIndex extends APersistentMap
        implements IObj, IKVReduce, IReduceInit, IEditableCollection {

    static final int LEAF_BITS = 4;
    static final int INNER_BITS = 6;
    static final int KEY_BITS = 22;
    static final int LEAF_MASK = (1 << LEAF_BITS) - 1;
    static final int INNER_MASK = (1 << INNER_BITS) - 1;
    static final int BIAS = (1 << 3) + (1 << 9) + (1 << 15) + (1 << 21);

    /// The index without chunks.
    public static final ChunkIndex EMPTY =
            new ChunkIndex(null, LEAF_BITS, -1, -1, 0, null, new Object(), null);

    final Object[] root;
    final int span, prefixU, prefixV, count;
    final IPersistentMap meta;
    final Object shape, token;

    ChunkIndex(
            Object[] root,
            int span,
            int prefixU,
            int prefixV,
            int count,
            IPersistentMap meta,
            Object shape,
            Object token
    ) {
        this.root = root;
        this.span = span;
        this.prefixU = prefixU;
        this.prefixV = prefixV;
        this.count = count;
        this.meta = meta;
        this.shape = shape;
        this.token = token;
    }

    /// Returns this index with a fresh owner token. Edits of the
    /// result and of their results copy each node once and then write
    /// it in place until `frozen()`.
    public ChunkIndex editable() {
        Object fresh = new Object();
        return new ChunkIndex(root, span, prefixU, prefixV, count, meta, shape, fresh);
    }

    /// Returns true when this index is open for a window of edits.
    public boolean editing() {
        return token != null;
    }

    /// Returns this index without an owner token, the same object
    /// when it has none.
    public ChunkIndex frozen() {
        if (token == null) return this;
        return new ChunkIndex(root, span, prefixU, prefixV, count, meta, shape, null);
    }

    /// Returns a token that is the same object for every index with
    /// this set of keys, however the values changed.
    public Object shape() {
        return shape;
    }

    static Object find(Object[] n, int span, int prefixU, int prefixV, int cx, int cz) {
        int u = cx + BIAS, v = cz + BIAS, s = span;
        if ((u >>> s) != prefixU || (v >>> s) != prefixV) return null;
        for (s -= INNER_BITS; s >= LEAF_BITS; s -= INNER_BITS) {
            n = (Object[]) n[slot(u, v, s)];
            if (n == null) return null;
        }
        return n[((u & LEAF_MASK) << LEAF_BITS) | (v & LEAF_MASK)];
    }

    static int slot(int u, int v, int s) {
        return (((u >>> s) & INNER_MASK) << INNER_BITS) | ((v >>> s) & INNER_MASK);
    }

    /// Returns the value of the chunk at `cx`, `cz`, or null.
    public Object get(int cx, int cz) {
        return find(root, span, prefixU, prefixV, cx, cz);
    }

    /// Returns the value of the chunk with key `id`.
    public Object get(long id) {
        return find(root, span, prefixU, prefixV, x(id), z(id));
    }

    /// Returns the map key for chunk coordinates cx and cz.
    public static long id(int cx, int cz) {
        return ((long) cx << 32) | (cz & 0xFFFFFFFFL);
    }

    /// Returns the chunk x of key `id`.
    public static int x(long id) {
        return (int) (id >> 32);
    }

    /// Returns the chunk z of key `id`.
    public static int z(long id) {
        return (int) id;
    }

    static boolean integral(Object k) {
        return k instanceof Long
                || k instanceof Integer
                || k instanceof Short
                || k instanceof Byte;
    }

    static long key(Object k) {
        if (integral(k)) return ((Number) k).longValue();
        throw new IllegalArgumentException("ChunkIndex key: " + k);
    }

    @Override

    public Object valAt(Object k, Object nf) {
        if (!integral(k)) return nf;
        Object o = get(((Number) k).longValue());
        return o == null ? nf : o;
    }

    @Override

    public Object valAt(Object k) {
        return valAt(k, null);
    }

    @Override

    public boolean containsKey(Object k) {
        return valAt(k, null) != null;
    }

    @Override

    public IMapEntry entryAt(Object k) {
        Object o = valAt(k, null);
        return o == null ? null : MapEntry.create(((Number) k).longValue(), o);
    }

    @Override

    public int count() {
        return count;
    }

    @Override

    public IPersistentMap meta() {
        return meta;
    }

    @Override

    public ChunkIndex withMeta(IPersistentMap m) {
        if (m == meta) return this;
        return new ChunkIndex(root, span, prefixU, prefixV, count, m, shape, token);
    }

    @Override

    public ChunkIndex empty() {
        return EMPTY.withMeta(meta);
    }

    @Override

    public ChunkIndex assoc(Object k, Object val) {
        long id = key(k);
        if (val != null && get(id) == val) return this;
        Edit e = new Edit(this);
        e.put(id, val);
        return e.done(meta);
    }

    @Override

    public ChunkIndex assocEx(Object k, Object val) {
        if (containsKey(k)) {
            throw Util.runtimeException("Key already present");
        }
        return assoc(k, val);
    }

    @Override

    public ChunkIndex without(Object k) {
        if (!containsKey(k)) return this;
        Edit e = new Edit(this);
        e.remove(((Number) k).longValue());
        return e.done(meta);
    }

    /// Returns this index with the block at x y z set to `state`, the
    /// same index when its chunk is absent or was written in place.
    public ChunkIndex withBlock(int x, int y, int z, int state) {
        Object c = find(root, span, prefixU, prefixV, x >> 4, z >> 4);
        if (c == null) return this;
        Chunk n = ((Chunk) c).withBlock(x, y, z, state, token);
        if (n == c) return this;
        return assoc(id(x >> 4, z >> 4), n);
    }

    /// Returns this index with each key of `ids` set to the value of
    /// `values` at the same place, removed where that value is null.
    public ChunkIndex withAll(long[] ids, Object[] values) {
        Edit e = new Edit(this);
        if (Edit.holes(values)) {
            e.mixed(ids, values);
        } else {
            for (int i = 0; i < ids.length; i++) {
                e.put(ids[i], values[i]);
            }
        }
        return e.done(meta);
    }

    @Override

    public ITransientMap asTransient() {
        return new Transient(new Edit(this), meta);
    }

    interface Sink {
        boolean put(long id, Object v);
    }

    void walk(Sink sink) {
        if (root == null) return;
        Walk w = new Walk(sink);
        w.nodes[0][0] = root;
        w.vps[0][0] = prefixV;
        w.size[0] = 1;
        w.rows(0, span, prefixU);
    }

    @Override

    public Object reduce(IFn f, Object init) {
        Object[] acc = {init};
        walk((id, v) -> {
            acc[0] = f.invoke(acc[0], MapEntry.create(id, v));
            return !RT.isReduced(acc[0]);
        });
        return unreduced(acc[0]);
    }

    @Override

    public Object kvreduce(IFn f, Object init) {
        Object[] acc = {init};
        walk((id, v) -> {
            acc[0] = f.invoke(acc[0], id, v);
            return !RT.isReduced(acc[0]);
        });
        return unreduced(acc[0]);
    }

    static Object unreduced(Object x) {
        return RT.isReduced(x) ? ((IDeref) x).deref() : x;
    }

    Object[] entries() {
        Object[] out = new Object[count];
        int[] at = {0};
        walk((id, v) -> {
            out[at[0]++] = MapEntry.create(id, v);
            return true;
        });
        return out;
    }

    @Override

    public ISeq seq() {
        return count == 0 ? null : ArraySeq.create(entries());
    }

    @Override

    public Iterator<Object> iterator() {
        return Arrays.asList(entries()).iterator();
    }

    static final class Walk {
        final Object[][][] nodes = new Object[4][][];
        final int[][] vps = new int[4][];
        final int[] size = new int[4];
        final Sink sink;

        Walk(Sink sink) {
            this.sink = sink;
            for (int d = 0; d < 4; d++) {
                nodes[d] = new Object[4][];
                vps[d] = new int[4];
            }
        }

        boolean rows(int d, int s, int up) {
            if (s == LEAF_BITS) return leaves(d, up);
            for (int ud = 0; ud <= INNER_MASK; ud++) {
                if (fill(d, ud) == 0) continue;
                if (!rows(d + 1, s - INNER_BITS, (up << INNER_BITS) | ud)) return false;
            }
            return true;
        }

        int fill(int d, int ud) {
            int m = 0;
            for (int i = 0; i < size[d]; i++) {
                Object[] n = nodes[d][i];
                int row = ud << INNER_BITS, vp = vps[d][i] << INNER_BITS;
                for (int vd = 0; vd <= INNER_MASK; vd++) {
                    Object kid = n[row | vd];
                    if (kid != null) m = add(d + 1, m, kid, vp | vd);
                }
            }
            return size[d + 1] = m;
        }

        int add(int d, int m, Object n, int vp) {
            if (m == nodes[d].length) {
                nodes[d] = Arrays.copyOf(nodes[d], m * 2);
                vps[d] = Arrays.copyOf(vps[d], m * 2);
            }
            nodes[d][m] = (Object[]) n;
            vps[d][m] = vp;
            return m + 1;
        }

        boolean leaves(int d, int up) {
            for (int ud = 0; ud <= LEAF_MASK; ud++) {
                long hi = ((((long) up << LEAF_BITS) | ud) - BIAS) << 32;
                int row = ud << LEAF_BITS;
                if (stopped(d, row, BIAS, Integer.MAX_VALUE, hi)
                        || stopped(d, row, 0, BIAS - 1, hi)) {
                    return false;
                }
            }
            return true;
        }

        boolean stopped(int d, int row, int from, int to, long hi) {
            for (int i = 0; i < size[d]; i++) {
                int base = vps[d][i] << LEAF_BITS;
                if (!cells(nodes[d][i], row, base, from, to, hi)) {
                    return true;
                }
            }
            return false;
        }

        boolean cells(Object[] n, int row, int base, int from, int to, long hi) {
            int a = Math.max(base, from) - base;
            int b = Math.min(base + LEAF_MASK, to) - base;
            for (int vd = a; vd <= b; vd++) {
                Object o = n[row | vd];
                long lo = (base + vd - BIAS) & 0xFFFFFFFFL;
                if (o != null && !sink.put(hi | lo, o)) return false;
            }
            return true;
        }
    }

    static final class Edit {
        final Object token, keep;
        final Object[][] path = new Object[4][];
        final int[] at = new int[4];
        Object[] root;
        int span, prefixU, prefixV, count;
        Object shape;

        Edit(ChunkIndex x) {
            root = x.root;
            span = x.span;
            prefixU = x.prefixU;
            prefixV = x.prefixV;
            count = x.count;
            shape = x.shape;
            keep = x.token;
            token = keep == null ? new Object() : keep;
        }

        ChunkIndex done(IPersistentMap meta) {
            if (root == null) return EMPTY.withMeta(meta);
            return new ChunkIndex(root, span, prefixU, prefixV, count, meta, shape, keep);
        }

        void reshaped() {
            shape = new Object();
        }

        Object get(long id) {
            return find(root, span, prefixU, prefixV, x(id), z(id));
        }

        Object[] own(Object[] n, int bits) {
            if (n == null) {
                n = new Object[(1 << (2 * bits)) + 1];
            } else if (n[n.length - 1] != token) {
                n = n.clone();
            } else {
                return n;
            }
            n[n.length - 1] = token;
            return n;
        }

        static int nodeBits(int s) {
            return s == LEAF_BITS ? LEAF_BITS : INNER_BITS;
        }

        void grow(int u, int v) {
            if (root == null) {
                span = LEAF_BITS;
                prefixU = u >>> LEAF_BITS;
                prefixV = v >>> LEAF_BITS;
            }
            while ((u >>> span) != prefixU || (v >>> span) != prefixV) {
                Object[] r = own(null, INNER_BITS);
                r[((prefixU & INNER_MASK) << INNER_BITS) | (prefixV & INNER_MASK)] = root;
                root = r;
                span += INNER_BITS;
                prefixU >>>= INNER_BITS;
                prefixV >>>= INNER_BITS;
            }
        }

        int descend(int u, int v) {
            Object[] n = root = own(root, nodeBits(span));
            int d = 0;
            for (int s = span - INNER_BITS; s >= LEAF_BITS; s -= INNER_BITS, d++) {
                int k = slot(u, v, s);
                Object[] kid = own((Object[]) n[k], nodeBits(s));
                path[d] = n;
                at[d] = k;
                n[k] = kid;
                n = kid;
            }
            path[d] = n;
            at[d] = ((u & LEAF_MASK) << LEAF_BITS) | (v & LEAF_MASK);
            return d;
        }

        Object[] leaf(int u, int v) {
            if ((u >>> span) != prefixU || (v >>> span) != prefixV) grow(u, v);
            Object[] n = root = own(root, nodeBits(span));
            for (int s = span - INNER_BITS; s >= LEAF_BITS; s -= INNER_BITS) {
                int k = slot(u, v, s);
                Object[] kid = own((Object[]) n[k], nodeBits(s));
                n[k] = kid;
                n = kid;
            }
            return n;
        }

        void put(long id, Object o) {
            int u = (int) (id >> 32) + BIAS, v = (int) id + BIAS;
            if (o == null || ((u | v) >>> KEY_BITS) != 0) throw bad(id, o);
            Object[] n = leaf(u, v);
            int k = ((u & LEAF_MASK) << LEAF_BITS) | (v & LEAF_MASK);
            if (n[k] == null) {
                count++;
                reshaped();
            }
            n[k] = o;
        }

        static boolean holes(Object[] values) {
            for (Object o : values) {
                if (o == null) return true;
            }
            return false;
        }

        void mixed(long[] ids, Object[] values) {
            for (int i = 0; i < ids.length; i++) {
                if (values[i] == null) remove(ids[i]);
                else put(ids[i], values[i]);
            }
        }

        static IllegalArgumentException bad(long id, Object o) {
            return new IllegalArgumentException("ChunkIndex entry: " + id + " " + o);
        }

        void remove(long id) {
            if (get(id) == null) return;
            int d = descend((int) (id >> 32) + BIAS, (int) id + BIAS);
            path[d][at[d]] = null;
            count--;
            reshaped();
            while (d > 0 && vacant(path[d])) path[--d][at[d]] = null;
            if (d == 0 && vacant(path[0])) {
                root = null;
                span = LEAF_BITS;
                prefixU = -1;
                prefixV = -1;
            }
        }

        static boolean vacant(Object[] n) {
            for (int i = 0; i < n.length - 1; i++) {
                if (n[i] != null) return false;
            }
            return true;
        }
    }

    static final class Transient extends AFn
            implements ITransientMap, ITransientAssociative2 {
        Edit edit;
        final IPersistentMap meta;

        Transient(Edit edit, IPersistentMap meta) {
            this.edit = edit;
            this.meta = meta;
        }

        Edit live() {
            if (edit == null) {
                throw new IllegalAccessError("Transient after persistent!");
            }
            return edit;
        }

        @Override

        public ITransientMap assoc(Object k, Object val) {
            live().put(key(k), val);
            return this;
        }

        @Override

        public ITransientMap without(Object k) {
            if (integral(k)) live().remove(((Number) k).longValue());
            return this;
        }

        @Override

        public ITransientMap conj(Object o) {
            if (o instanceof Map.Entry<?, ?> e) {
                return assoc(e.getKey(), e.getValue());
            }
            if (o instanceof IPersistentVector v && v.count() == 2) {
                return assoc(v.nth(0), v.nth(1));
            }
            throw new IllegalArgumentException("ChunkIndex conj: " + o);
        }

        @Override

        public IPersistentMap persistent() {
            ChunkIndex x = live().done(meta);
            edit = null;
            return x;
        }

        @Override

        public Object valAt(Object k, Object nf) {
            Object o = integral(k) ? live().get(((Number) k).longValue()) : null;
            return o == null ? nf : o;
        }

        @Override

        public Object valAt(Object k) {
            return valAt(k, null);
        }

        @Override

        public int count() {
            return live().count;
        }

        @Override

        public boolean containsKey(Object k) {
            return valAt(k, null) != null;
        }

        @Override

        public IMapEntry entryAt(Object k) {
            Object o = valAt(k, null);
            return o == null ? null : MapEntry.create(((Number) k).longValue(), o);
        }

        @Override

        public Object invoke(Object k) {
            return valAt(k, null);
        }

        @Override

        public Object invoke(Object k, Object nf) {
            return valAt(k, nf);
        }
    }
}
