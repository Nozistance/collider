package collider.data;

import clojure.lang.AFn;
import clojure.lang.APersistentMap;
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
import clojure.lang.ITransientCollection;
import clojure.lang.ITransientMap;
import clojure.lang.MapEntry;
import clojure.lang.RT;
import clojure.lang.Reversible;
import clojure.lang.Util;
import java.util.Iterator;
import java.util.Map;

/// A persistent map from long keys to values, in signed key order.
///
/// Two maps with the same keys have the same shape. Edits that change
/// nothing return the same map, and merges keep shared parts. Values
/// are never nil.
public final class LongMap extends APersistentMap
        implements IObj, IEditableCollection, IKVReduce, IReduceInit, Reversible {

    public static final LongMap EMPTY = new LongMap(null, null);

    final Node root;
    final IPersistentMap meta;

    LongMap(Node root, IPersistentMap meta) {
        this.root = root;
        this.meta = meta;
    }

    LongMap with(Node n) {
        if (n == root) return this;
        return new LongMap(n, meta);
    }

    /// Returns the value at key `k`, or null when the key is absent.
    public Object get(long k) {
        long u = Node.u(k);
        Node l = Node.leafOf(root, u);
        return l == null ? null : l.val(u);
    }

    /// Returns the value at key `k`, or `nf` when the key is absent.
    public Object get(long k, Object nf) {
        Object v = get(k);
        return v == null ? nf : v;
    }

    /// Returns true when the map has key `k`.
    public boolean has(long k) {
        return Node.leafOf(root, Node.u(k)) != null;
    }

    /// Returns the map with `v` at key `k`. Throws when `v` is nil.
    public LongMap put(long k, Object v) {
        return with(Node.put(root, Node.u(k), Node.value(v), null));
    }

    /// Returns the map without key `k`.
    public LongMap remove(long k) {
        return with(Node.remove(root, Node.u(k), null));
    }

    /// Returns the least key. Throws when the map is empty.
    public long first() {
        if (root == null) throw new IllegalStateException("Empty map has no first key");
        return Node.first(root);
    }

    /// Returns the greatest key. Throws when the map is empty.
    public long last() {
        if (root == null) throw new IllegalStateException("Empty map has no last key");
        return Node.last(root);
    }

    /// Returns the entries with keys from `lo` to `hi`, both included.
    public LongMap range(long lo, long hi) {
        return with(root == null ? null : Node.range(root, Node.u(lo), Node.u(hi)));
    }

    /// Returns the entries of both maps, values of `o` on shared keys.
    public LongMap merge(LongMap o) {
        return with(Node.combine(Node.UNION, root, o.root, null));
    }

    /// Returns the entries of both maps, `(f this-value o-value)` on
    /// shared keys. Throws when `f` returns nil.
    public LongMap merge(LongMap o, IFn f) {
        return with(Node.combine(Node.UNION, root, o.root, f));
    }

    /// Returns the set of keys.
    @Override
    public LongSet keySet() {
        return root == null ? LongSet.EMPTY : new LongSet(Node.keys(root), null);
    }

    /// Reduces `(f acc k old new)` over the keys whose values differ
    /// between this map and `o`, in key order. An absent value is nil.
    /// Shared parts cost nothing.
    public Object diff(LongMap o, IFn f, Object init) {
        return Node.unreduced(Node.diff(root, o.root, f, init, false));
    }

    /// Reduces the map in parts of at most `n` entries in parallel,
    /// with `(reducef acc k v)`, and joins the parts in key order with
    /// `combinef`.
    public Object fold(int n, IFn combinef, IFn reducef) {
        return Node.fold(root, n, combinef, reducef, Node.KV);
    }

    /// Returns the map of keys `ks` to values `vs`.
    public static LongMap fromSorted(long[] ks, Object[] vs) {
        return EMPTY.with(Node.fromSorted(ks, vs));
    }

    /// Returns the keys in order.
    public long[] keys() {
        long[] ks = new long[count()];
        int i = 0;
        for (Iterator<Object> it = new Node.Walk(root, false, false); it.hasNext(); ) ks[i++] = (Long) it.next();
        return ks;
    }

    /// Returns the values in key order.
    public Object[] vals() {
        Object[] vs = new Object[count()];
        int i = 0;
        for (Iterator<Object> it = new Node.Walk(root, true, false); it.hasNext(); ) {
            vs[i++] = ((IMapEntry) it.next()).val();
        }
        return vs;
    }

    /// Throws when the trie breaks one of its shape invariants.
    public void check() {
        Node.need(root == null || Node.check(root, 64, true) > 0, "root");
    }

    @Override
    public Object valAt(Object k, Object nf) {
        return Node.integral(k) ? get(((Number) k).longValue(), nf) : nf;
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
        Object v = valAt(k, null);
        return v == null ? null : MapEntry.create(((Number) k).longValue(), v);
    }

    @Override
    public int count() {
        return root == null ? 0 : root.count;
    }

    @Override
    public LongMap assoc(Object k, Object v) {
        return put(Node.key(k), v);
    }

    @Override
    public LongMap assocEx(Object k, Object v) {
        if (containsKey(k)) throw Util.runtimeException("Key already present");
        return assoc(k, v);
    }

    @Override
    public LongMap without(Object k) {
        return Node.integral(k) ? remove(((Number) k).longValue()) : this;
    }

    @Override
    public ISeq seq() {
        return RT.chunkIteratorSeq(iterator());
    }

    @Override
    public Iterator<Object> iterator() {
        return new Node.Walk(root, true, false);
    }

    @Override
    public ISeq rseq() {
        return RT.chunkIteratorSeq(new Node.Walk(root, true, true));
    }

    @Override
    public LongMap empty() {
        return EMPTY.withMeta(meta);
    }

    @Override
    public IPersistentMap meta() {
        return meta;
    }

    @Override
    public LongMap withMeta(IPersistentMap m) {
        if (m == meta) return this;
        return new LongMap(root, m);
    }

    @Override
    public Object kvreduce(IFn f, Object init) {
        return root == null ? init : Node.unreduced(Node.reduce(root, Node.KV, f, init));
    }

    @Override
    public Object reduce(IFn f, Object init) {
        return root == null ? init : Node.unreduced(Node.reduce(root, Node.ENTRIES, f, init));
    }

    @Override
    public ITransientMap asTransient() {
        return new Transient(this);
    }

    static final class Transient extends AFn implements ITransientMap, ITransientAssociative2 {
        final LongMap from;
        Node root;
        Object edit = new Object();

        Transient(LongMap from) {
            this.from = from;
            this.root = from.root;
        }

        Object edit() {
            if (edit == null) throw new IllegalAccessError("Transient used after persistent!");
            return edit;
        }

        @Override
        public Transient assoc(Object k, Object v) {
            root = Node.put(root, Node.u(Node.key(k)), Node.value(v), edit());
            return this;
        }

        @Override
        public Transient without(Object k) {
            Object e = edit();
            if (Node.integral(k)) root = Node.remove(root, Node.u(((Number) k).longValue()), e);
            return this;
        }

        @Override
        public ITransientCollection conj(Object o) {
            edit();
            if (o instanceof Map.Entry<?, ?> e) return assoc(e.getKey(), e.getValue());
            if (o instanceof IPersistentVector v) {
                if (v.count() != 2) throw new IllegalArgumentException("Vector arg to map conj must be a pair");
                return assoc(v.nth(0), v.nth(1));
            }
            for (ISeq s = RT.seq(o); s != null; s = s.next()) {
                Map.Entry<?, ?> e = (Map.Entry<?, ?>) s.first();
                assoc(e.getKey(), e.getValue());
            }
            return this;
        }

        @Override
        public LongMap persistent() {
            edit();
            edit = null;
            return from.with(root);
        }

        @Override
        public Object valAt(Object k, Object nf) {
            edit();
            if (!Node.integral(k)) return nf;
            long u = Node.u(((Number) k).longValue());
            Node l = Node.leafOf(root, u);
            return l == null ? nf : l.val(u);
        }

        @Override
        public Object valAt(Object k) {
            return valAt(k, null);
        }

        @Override
        public int count() {
            edit();
            return root == null ? 0 : root.count;
        }

        @Override
        public boolean containsKey(Object k) {
            return valAt(k, null) != null;
        }

        @Override
        public IMapEntry entryAt(Object k) {
            Object v = valAt(k, null);
            return v == null ? null : MapEntry.create(((Number) k).longValue(), v);
        }

        @Override
        public Object invoke(Object k) {
            return valAt(k);
        }

        @Override
        public Object invoke(Object k, Object nf) {
            return valAt(k, nf);
        }
    }
}
