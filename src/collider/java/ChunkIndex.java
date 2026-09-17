package collider.java;

import clojure.lang.*;

import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;

/// A persistent map from chunk coordinates to chunk values.
public final class ChunkIndex extends APersistentMap
        implements IObj, IKVReduce, IReduceInit, IEditableCollection {

    static final int L = 4, I = 6, D = 22;
    static final int LM = (1 << L) - 1, IM = (1 << I) - 1;
    static final int B = (1 << 3) + (1 << 9) + (1 << 15) + (1 << 21);
    public static final ChunkIndex EMPTY =
            new ChunkIndex(null, L, -1, -1, 0, null);

    final Object[] root;
    final int span, pu, pv, count;
    final IPersistentMap meta;

    ChunkIndex(Object[] root, int span, int pu, int pv, int count,
               IPersistentMap meta) {
        this.root = root;
        this.span = span;
        this.pu = pu;
        this.pv = pv;
        this.count = count;
        this.meta = meta;
    }

    static Object find(Object[] n, int span, int pu, int pv,
                       int cx, int cz) {
        int u = cx + B, v = cz + B, s = span;
        if ((u >>> s) != pu || (v >>> s) != pv) return null;
        for (s -= I; s >= L; s -= I) {
            n = (Object[]) n[slot(u, v, s)];
            if (n == null) return null;
        }
        return n[((u & LM) << L) | (v & LM)];
    }

    static int slot(int u, int v, int s) {
        return (((u >>> s) & IM) << I) | ((v >>> s) & IM);
    }

    public Object get(int cx, int cz) {
        return find(root, span, pu, pv, cx, cz);
    }

    /// Returns the value at the key `id(cx, cz)` gives.
    public Object get(long id) {
        return find(root, span, pu, pv, (int) (id >> 32), (int) id);
    }

    /// Returns the map key for chunk coordinates cx and cz.
    public static long id(int cx, int cz) {
        return ((long) cx << 32) | (cz & 0xFFFFFFFFL);
    }

    static boolean integral(Object k) {
        return k instanceof Long || k instanceof Integer
                || k instanceof Short || k instanceof Byte;
    }

    static long key(Object k) {
        if (integral(k)) return ((Number) k).longValue();
        throw new IllegalArgumentException("ChunkIndex key: " + k);
    }

    public Object valAt(Object k, Object nf) {
        if (!integral(k)) return nf;
        Object o = get(((Number) k).longValue());
        return o == null ? nf : o;
    }

    public Object valAt(Object k) {
        return valAt(k, null);
    }

    public boolean containsKey(Object k) {
        return valAt(k, null) != null;
    }

    public IMapEntry entryAt(Object k) {
        Object o = valAt(k, null);
        return o == null ? null
                : MapEntry.create(((Number) k).longValue(), o);
    }

    public int count() {
        return count;
    }

    public IPersistentMap meta() {
        return meta;
    }

    public ChunkIndex withMeta(IPersistentMap m) {
        if (m == meta) return this;
        return new ChunkIndex(root, span, pu, pv, count, m);
    }

    public ChunkIndex empty() {
        return EMPTY.withMeta(meta);
    }

    public ChunkIndex assoc(Object k, Object val) {
        long id = key(k);
        if (val != null && get(id) == val) return this;
        Edit e = new Edit(this);
        e.put(id, val);
        return e.done(meta);
    }

    public ChunkIndex assocEx(Object k, Object val) {
        if (containsKey(k)) {
            throw Util.runtimeException("Key already present");
        }
        return assoc(k, val);
    }

    public ChunkIndex without(Object k) {
        if (!containsKey(k)) return this;
        Edit e = new Edit(this);
        e.remove(((Number) k).longValue());
        return e.done(meta);
    }

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

    public ITransientMap asTransient() {
        return new Transient(new Edit(this), meta);
    }

    interface Sink {
        boolean put(long id, Object v);
    }

    boolean walk(Sink sink) {
        if (root == null) return true;
        Walk w = new Walk(sink);
        w.nodes[0][0] = root;
        w.vps[0][0] = pv;
        w.size[0] = 1;
        return w.rows(0, span, pu);
    }

    public Object reduce(IFn f, Object init) {
        Object[] acc = {init};
        walk((id, v) -> {
            acc[0] = f.invoke(acc[0], MapEntry.create(id, v));
            return !RT.isReduced(acc[0]);
        });
        return unreduced(acc[0]);
    }

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

    public ISeq seq() {
        return count == 0 ? null : ArraySeq.create(entries());
    }

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
            if (s == L) return leaves(d, up);
            for (int ud = 0; ud <= IM; ud++) {
                if (fill(d, ud) == 0) continue;
                if (!rows(d + 1, s - I, (up << I) | ud)) return false;
            }
            return true;
        }

        int fill(int d, int ud) {
            int m = 0;
            for (int i = 0; i < size[d]; i++) {
                Object[] n = nodes[d][i];
                int row = ud << I, vp = vps[d][i] << I;
                for (int vd = 0; vd <= IM; vd++) {
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
            for (int ud = 0; ud <= LM; ud++) {
                long hi = ((((long) up << L) | ud) - B) << 32;
                int row = ud << L;
                if (!pass(d, row, B, Integer.MAX_VALUE, hi)
                        || !pass(d, row, 0, B - 1, hi)) return false;
            }
            return true;
        }

        boolean pass(int d, int row, int from, int to, long hi) {
            for (int i = 0; i < size[d]; i++) {
                int base = vps[d][i] << L;
                if (!cells(nodes[d][i], row, base, from, to, hi)) {
                    return false;
                }
            }
            return true;
        }

        boolean cells(Object[] n, int row, int base, int from, int to,
                      long hi) {
            int a = Math.max(base, from) - base;
            int b = Math.min(base + LM, to) - base;
            for (int vd = a; vd <= b; vd++) {
                Object o = n[row | vd];
                long lo = (base + vd - B) & 0xFFFFFFFFL;
                if (o != null && !sink.put(hi | lo, o)) return false;
            }
            return true;
        }
    }

    static final class Edit {
        final Object token = new Object();
        final Object[][] path = new Object[4][];
        final int[] at = new int[4];
        Object[] root;
        int span, pu, pv, count;

        Edit(ChunkIndex x) {
            root = x.root;
            span = x.span;
            pu = x.pu;
            pv = x.pv;
            count = x.count;
        }

        ChunkIndex done(IPersistentMap meta) {
            if (root == null) return EMPTY.withMeta(meta);
            return new ChunkIndex(root, span, pu, pv, count, meta);
        }

        Object get(long id) {
            int cx = (int) (id >> 32), cz = (int) id;
            return find(root, span, pu, pv, cx, cz);
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

        void grow(int u, int v) {
            if (root == null) {
                span = L;
                pu = u >>> L;
                pv = v >>> L;
            }
            while ((u >>> span) != pu || (v >>> span) != pv) {
                Object[] r = own(null, I);
                r[((pu & IM) << I) | (pv & IM)] = root;
                root = r;
                span += I;
                pu >>>= I;
                pv >>>= I;
            }
        }

        int descend(int u, int v) {
            Object[] n = root = own(root, span == L ? L : I);
            int d = 0;
            for (int s = span - I; s >= L; s -= I, d++) {
                int k = slot(u, v, s);
                Object[] kid = own((Object[]) n[k], s == L ? L : I);
                path[d] = n;
                at[d] = k;
                n[k] = kid;
                n = kid;
            }
            path[d] = n;
            at[d] = ((u & LM) << L) | (v & LM);
            return d;
        }

        Object[] leaf(int u, int v) {
            if ((u >>> span) != pu || (v >>> span) != pv) grow(u, v);
            Object[] n = root = own(root, span == L ? L : I);
            for (int s = span - I; s >= L; s -= I) {
                int k = slot(u, v, s);
                Object[] kid = own((Object[]) n[k], s == L ? L : I);
                n[k] = kid;
                n = kid;
            }
            return n;
        }

        void put(long id, Object o) {
            int u = (int) (id >> 32) + B, v = (int) id + B;
            if (o == null || ((u | v) >>> D) != 0) throw bad(id, o);
            Object[] n = leaf(u, v);
            int k = ((u & LM) << L) | (v & LM);
            if (n[k] == null) count++;
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
            return new IllegalArgumentException(
                    "ChunkIndex entry: " + id + " " + o);
        }

        void remove(long id) {
            if (get(id) == null) return;
            int d = descend((int) (id >> 32) + B, (int) id + B);
            path[d][at[d]] = null;
            count--;
            while (d > 0 && vacant(path[d])) path[--d][at[d]] = null;
            if (d == 0 && vacant(path[0])) {
                root = null;
                span = L;
                pu = -1;
                pv = -1;
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
                throw new IllegalAccessError(
                        "Transient after persistent!");
            }
            return edit;
        }

        public ITransientMap assoc(Object k, Object val) {
            live().put(key(k), val);
            return this;
        }

        public ITransientMap without(Object k) {
            if (integral(k)) live().remove(((Number) k).longValue());
            return this;
        }

        public ITransientMap conj(Object o) {
            if (o instanceof Map.Entry<?, ?> e) {
                return assoc(e.getKey(), e.getValue());
            }
            if (o instanceof IPersistentVector v && v.count() == 2) {
                return assoc(v.nth(0), v.nth(1));
            }
            throw new IllegalArgumentException(
                    "ChunkIndex conj: " + o);
        }

        public IPersistentMap persistent() {
            ChunkIndex x = live().done(meta);
            edit = null;
            return x;
        }

        public Object valAt(Object k, Object nf) {
            Object o = integral(k)
                    ? live().get(((Number) k).longValue()) : null;
            return o == null ? nf : o;
        }

        public Object valAt(Object k) {
            return valAt(k, null);
        }

        public int count() {
            return live().count;
        }

        public boolean containsKey(Object k) {
            return valAt(k, null) != null;
        }

        public IMapEntry entryAt(Object k) {
            Object o = valAt(k, null);
            return o == null ? null
                    : MapEntry.create(((Number) k).longValue(), o);
        }

        public Object invoke(Object k) {
            return valAt(k, null);
        }

        public Object invoke(Object k, Object nf) {
            return valAt(k, nf);
        }
    }
}
