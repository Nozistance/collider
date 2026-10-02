package longmap;

import clojure.lang.AFn;
import clojure.lang.APersistentSet;
import clojure.lang.IEditableCollection;
import clojure.lang.IFn;
import clojure.lang.IObj;
import clojure.lang.IPersistentMap;
import clojure.lang.IReduceInit;
import clojure.lang.ISeq;
import clojure.lang.ITransientSet;
import clojure.lang.RT;
import clojure.lang.Reversible;
import java.util.Iterator;

/// A persistent set of longs, in signed order.
///
/// Two sets with the same keys have the same shape. Edits that change
/// nothing return the same set, and set algebra keeps shared parts.
public final class LongSet extends APersistentSet implements IObj, IEditableCollection, IReduceInit, Reversible {

    public static final LongSet EMPTY = new LongSet(null, null);

    final Node root;
    final IPersistentMap meta;

    LongSet(Node root, IPersistentMap meta) {
        super(null);
        this.root = root;
        this.meta = meta;
    }

    LongSet with(Node n) {
        if (n == root) return this;
        return new LongSet(n, meta);
    }

    /// Returns true when the set has `k`.
    public boolean has(long k) {
        return Node.leafOf(root, Node.u(k)) != null;
    }

    /// Returns the set with `k`.
    public LongSet add(long k) {
        return with(Node.put(root, Node.u(k), null, null));
    }

    /// Returns the set without `k`.
    public LongSet remove(long k) {
        return with(Node.remove(root, Node.u(k), null));
    }

    /// Returns the least key. Throws when the set is empty.
    public long first() {
        if (root == null) throw new IllegalStateException("Empty set has no first key");
        return Node.first(root);
    }

    /// Returns the greatest key. Throws when the set is empty.
    public long last() {
        if (root == null) throw new IllegalStateException("Empty set has no last key");
        return Node.last(root);
    }

    /// Returns the keys from `lo` to `hi`, both included.
    public LongSet range(long lo, long hi) {
        return with(root == null ? null : Node.range(root, Node.u(lo), Node.u(hi)));
    }

    /// Returns the keys in either set. Returns this set itself when
    /// `o` adds nothing.
    public LongSet union(LongSet o) {
        return with(Node.combine(Node.UNION, root, o.root, null));
    }

    /// Returns the keys in both sets.
    public LongSet intersection(LongSet o) {
        return with(Node.combine(Node.INTER, root, o.root, null));
    }

    /// Returns the keys of this set that are not in `o`.
    public LongSet difference(LongSet o) {
        return with(Node.combine(Node.DIFF, root, o.root, null));
    }

    /// Reduces `(f acc k added)` in key order over the keys in exactly
    /// one of this set and `o`. `added` is true for keys of `o`.
    public Object diff(LongSet o, IFn f, Object init) {
        return Node.unreduced(Node.diff(root, o.root, f, init, true));
    }

    /// Reduces the set in parts of at most `n` keys in parallel, with
    /// `(reducef acc k)`, and joins the parts in key order with
    /// `combinef`.
    public Object fold(int n, IFn combinef, IFn reducef) {
        return Node.fold(root, n, combinef, reducef, Node.KEYS);
    }

    /// Returns the set of keys `ks`.
    public static LongSet fromSorted(long[] ks) {
        return EMPTY.with(Node.fromSorted(ks, null));
    }

    /// Returns the keys in order.
    public long[] toLongArray() {
        long[] ks = new long[count()];
        int i = 0;
        for (Iterator<Object> it = iterator(); it.hasNext(); ) ks[i++] = (Long) it.next();
        return ks;
    }

    /// Throws when the trie breaks one of its shape invariants.
    public void check() {
        Node.need(root == null || Node.check(root, 64, false) > 0, "root");
    }

    @Override
    public boolean contains(Object k) {
        return Node.integral(k) && has(((Number) k).longValue());
    }

    @Override
    public Object get(Object k) {
        return contains(k) ? k : null;
    }

    @Override
    public int count() {
        return root == null ? 0 : root.count;
    }

    @Override
    public ISeq seq() {
        return RT.chunkIteratorSeq(iterator());
    }

    @Override
    public Iterator<Object> iterator() {
        return new Node.Walk(root, false, false);
    }

    @Override
    public ISeq rseq() {
        return RT.chunkIteratorSeq(new Node.Walk(root, false, true));
    }

    @Override
    public LongSet disjoin(Object k) {
        return Node.integral(k) ? remove(((Number) k).longValue()) : this;
    }

    @Override
    public LongSet cons(Object k) {
        return add(Node.key(k));
    }

    @Override
    public LongSet empty() {
        return EMPTY.withMeta(meta);
    }

    @Override
    public IPersistentMap meta() {
        return meta;
    }

    @Override
    public LongSet withMeta(IPersistentMap m) {
        if (m == meta) return this;
        return new LongSet(root, m);
    }

    @Override
    public Object reduce(IFn f, Object init) {
        return root == null ? init : Node.unreduced(Node.reduce(root, Node.KEYS, f, init));
    }

    @Override
    public ITransientSet asTransient() {
        return new Transient(this);
    }

    static final class Transient extends AFn implements ITransientSet {
        final LongSet from;
        Node root;
        Object edit = new Object();

        Transient(LongSet from) {
            this.from = from;
            this.root = from.root;
        }

        Object edit() {
            if (edit == null) throw new IllegalAccessError("Transient used after persistent!");
            return edit;
        }

        @Override
        public Transient conj(Object k) {
            root = Node.put(root, Node.u(Node.key(k)), null, edit());
            return this;
        }

        @Override
        public Transient disjoin(Object k) {
            Object e = edit();
            if (Node.integral(k)) root = Node.remove(root, Node.u(((Number) k).longValue()), e);
            return this;
        }

        @Override
        public LongSet persistent() {
            edit();
            edit = null;
            return from.with(root);
        }

        @Override
        public boolean contains(Object k) {
            edit();
            return Node.integral(k) && Node.leafOf(root, Node.u(((Number) k).longValue())) != null;
        }

        @Override
        public Object get(Object k) {
            return contains(k) ? k : null;
        }

        @Override
        public int count() {
            edit();
            return root == null ? 0 : root.count;
        }

        @Override
        public Object invoke(Object k) {
            return get(k);
        }

        @Override
        public Object invoke(Object k, Object nf) {
            return contains(k) ? k : nf;
        }
    }
}
