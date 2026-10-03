package collider.data;

import clojure.lang.AFn;
import clojure.lang.IFn;
import clojure.lang.MapEntry;
import clojure.lang.RT;
import clojure.lang.Reduced;
import java.util.ArrayDeque;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Iterator;
import java.util.NoSuchElementException;

/// A node of the radix trie under LongMap and LongSet.
///
/// Keys enter as `u = k ^ Long.MIN_VALUE`, so unsigned order on `u`
/// is signed order on `k`. A node at `shift` covers the keys whose
/// bits above `shift + BITS` equal those of `base`. A leaf has shift 0
/// and one bit per key. A branch has one bit per child slot. A set
/// node has no values. The shape depends on the key set alone.
///
/// A node with a non-null `edit` belongs to one transient. Only that
/// transient writes it in place, and only while it is open.
final class Node {

    /// Bits of key per trie level. A leaf holds `1 << BITS` keys.
    static final int BITS = 6;

    static final int MASK = (1 << BITS) - 1;
    static final int UNION = 0, INTER = 1, DIFF = 2;
    static final int KEYS = 0, KV = 1, ENTRIES = 2, VALS = 3;
    static final Comparator<Object> ORDER = Comparator.comparingLong(Node::key);

    final Object edit;
    final long base;
    final int shift;
    int count;
    long bits;
    Node[] kids;
    Object[] vals;

    Node(
            Object edit,
            long base,
            int shift,
            int count,
            long bits,
            Node[] kids,
            Object[] vals
    ) {
        this.edit = edit;
        this.base = base;
        this.shift = shift;
        this.count = count;
        this.bits = bits;
        this.kids = kids;
        this.vals = vals;
    }

    static long u(long k) {
        return k ^ Long.MIN_VALUE;
    }

    static boolean integral(Object k) {
        return k instanceof Long
                || k instanceof Integer
                || k instanceof Short
                || k instanceof Byte;
    }

    static long key(Object k) {
        if (integral(k)) return ((Number) k).longValue();
        throw new IllegalArgumentException("Key is not an integer: " + k);
    }

    static Object value(Object v) {
        if (v != null) return v;
        throw new IllegalArgumentException("Value is nil");
    }

    static int slot(long u, int shift) {
        return (int) (u >>> shift) & MASK;
    }

    static int index(long bits, long bit) {
        return Long.bitCount(bits & (bit - 1));
    }

    static long above(int shift) {
        return (-1L << shift) << BITS;
    }

    boolean covers(long u) {
        return ((u ^ base) >>> shift) >>> BITS == 0;
    }

    long bit(long u) {
        return 1L << slot(u, shift);
    }

    long keyAt(long bit) {
        return u(base | Long.numberOfTrailingZeros(bit));
    }

    boolean owned(Object e) {
        return e != null && edit == e;
    }

    static Node leaf(long u, Object v, Object edit) {
        Object[] vs = v == null ? null : new Object[] {v};
        return new Node(edit, u & above(0), 0, 1, 1L << slot(u, 0), null, vs);
    }

    static Node leafOf(Node n, long u) {
        while (n != null && n.covers(u)) {
            long bit = n.bit(u);
            if ((n.bits & bit) == 0) return null;
            if (n.shift == 0) return n;
            n = n.kids[index(n.bits, bit)];
        }
        return null;
    }

    Object val(long u) {
        return vals == null ? Boolean.TRUE : vals[index(bits, bit(u))];
    }

    static Node join(Node a, Node b, Object edit) {
        int h = 63 - Long.numberOfLeadingZeros(a.base ^ b.base);
        int s = h / BITS * BITS;
        int sa = slot(a.base, s), sb = slot(b.base, s);
        Node[] ks = sa < sb ? new Node[] {a, b} : new Node[] {b, a};
        long bits = (1L << sa) | (1L << sb);
        return new Node(edit, a.base & above(s), s, a.count + b.count, bits, ks, null);
    }

    static <T> T[] inserted(T[] a, int i, T x) {
        T[] r = Arrays.copyOf(a, a.length + 1);
        System.arraycopy(a, i, r, i + 1, a.length - i);
        r[i] = x;
        return r;
    }

    static <T> T[] removed(T[] a, int i) {
        T[] r = Arrays.copyOf(a, a.length - 1);
        System.arraycopy(a, i + 1, r, i, a.length - i - 1);
        return r;
    }

    Node insert(long bit, int i, Node kid, Object v, Object e) {
        int c = count + (kid == null ? 1 : kid.count);
        Node[] ks = kid == null ? null : inserted(kids, i, kid);
        Object[] vs = vals == null ? null : inserted(vals, i, v);
        if (!owned(e)) return new Node(e, base, shift, c, bits | bit, ks, vs);
        count = c;
        bits |= bit;
        kids = ks;
        vals = vs;
        return this;
    }

    Node replace(int i, Node kid, Object v, int delta, Object e) {
        Node n = owned(e)
                ? this
                : new Node(e, base, shift, count, bits, clone(kids), clone(vals));
        if (kid != null) n.kids[i] = kid;
        else n.vals[i] = v;
        n.count += delta;
        return n;
    }

    Node drop(long bit, int i, int gone, Object e) {
        Node[] ks = kids == null ? null : removed(kids, i);
        Object[] vs = vals == null ? null : removed(vals, i);
        if (!owned(e)) return new Node(e, base, shift, count - gone, bits & ~bit, ks, vs);
        count -= gone;
        bits &= ~bit;
        kids = ks;
        vals = vs;
        return this;
    }

    static <T> T[] clone(T[] a) {
        return a == null ? null : a.clone();
    }

    static Node put(Node n, long u, Object v, Object e) {
        if (n == null) return leaf(u, v, e);
        if (!n.covers(u)) return join(n, leaf(u, v, e), e);
        long bit = n.bit(u);
        int i = index(n.bits, bit);
        if ((n.bits & bit) == 0) {
            return n.insert(bit, i, n.shift == 0 ? null : leaf(u, v, e), v, e);
        }
        if (n.shift == 0) {
            return v == null || n.vals[i] == v ? n : n.replace(i, null, v, 0, e);
        }
        Node k = n.kids[i];
        int c = k.count;
        Node k2 = put(k, u, v, e);
        return k2 == k && k2.count == c ? n : n.replace(i, k2, null, k2.count - c, e);
    }

    static Node remove(Node n, long u, Object e) {
        if (n == null || !n.covers(u)) return n;
        long bit = n.bit(u);
        if ((n.bits & bit) == 0) return n;
        int i = index(n.bits, bit);
        if (n.shift == 0) return n.bits == bit ? null : n.drop(bit, i, 1, e);
        Node k = n.kids[i];
        int c = k.count;
        Node k2 = remove(k, u, e);
        if (k2 == k && k.count == c) return n;
        if (k2 != null) return n.replace(i, k2, null, k2.count - c, e);
        return Long.bitCount(n.bits) == 2 ? n.kids[1 - i] : n.drop(bit, i, c, e);
    }

    static boolean apart(Node top, Node n) {
        return top.shift > n.shift
                ? !top.covers(n.base)
                : top.shift != n.shift || top.base != n.base;
    }

    static long slots(Node n, int s) {
        return n.shift == s ? n.bits : 1L << slot(n.base, s);
    }

    static Node kid(Node n, int s, long ns, long bit) {
        if ((ns & bit) == 0) return null;
        return n.shift == s ? n.kids[index(ns, bit)] : n;
    }

    static Node combine(int op, Node a, Node b, IFn f) {
        if (a == null) return op == UNION ? b : null;
        if (b == null) return op == INTER ? null : a;
        if (a == b && f == null) return op == DIFF ? null : a;
        Node top = a.shift >= b.shift ? a : b;
        if (apart(top, top == a ? b : a)) {
            return op == UNION ? join(a, b, null) : op == INTER ? null : a;
        }
        if (top.shift == 0) return leaves(op, a, b, f);
        int s = top.shift;
        long as = slots(a, s), bs = slots(b, s), all = as | bs;
        Node[] ks = new Node[Long.bitCount(all)];
        long bm = 0;
        int n = 0;
        for (long r = all; r != 0; r &= r - 1) {
            long bit = r & -r;
            Node k = combine(op, kid(a, s, as, bit), kid(b, s, bs, bit), f);
            if (k == null) continue;
            ks[n++] = k;
            bm |= bit;
        }
        return pack(top.base, s, bm, ks, n, a, b);
    }

    static Node pack(long base, int s, long bm, Node[] ks, int n, Node a, Node b) {
        if (n < 2) return n == 0 ? null : ks[0];
        if (same(a, s, bm, ks, n)) return a;
        if (same(b, s, bm, ks, n)) return b;
        int c = 0;
        for (int i = 0; i < n; i++) c += ks[i].count;
        return new Node(null, base, s, c, bm, Arrays.copyOf(ks, n), null);
    }

    static boolean same(Node x, int s, long bm, Node[] ks, int n) {
        return x != null
                && x.shift == s
                && x.bits == bm
                && Arrays.equals(x.kids, 0, n, ks, 0, n);
    }

    static Node leaves(int op, Node a, Node b, IFn f) {
        long bits = switch (op) {
            case UNION -> a.bits | b.bits;
            case INTER -> a.bits & b.bits;
            default -> a.bits & ~b.bits;
        };
        if (bits == 0) return null;
        if (a.vals == null) {
            return bits == a.bits ? a : bits == b.bits ? b : a.restrict(bits);
        }
        Object[] vs = new Object[Long.bitCount(bits)];
        boolean sameA = bits == a.bits, sameB = bits == b.bits;
        int i = 0;
        for (long r = bits; r != 0; r &= r - 1, i++) {
            long bit = r & -r;
            Object va = (a.bits & bit) == 0 ? null : a.vals[index(a.bits, bit)];
            Object vb = (b.bits & bit) == 0 ? null : b.vals[index(b.bits, bit)];
            Object v = merged(op, va, vb, f);
            sameA &= v == va;
            sameB &= v == vb;
            vs[i] = v;
        }
        if (sameA) return a;
        if (sameB) return b;
        return new Node(null, a.base, 0, vs.length, bits, null, vs);
    }

    static Object merged(int op, Object va, Object vb, IFn f) {
        if (va == null) return vb;
        if (vb == null || op != UNION) return va;
        return f == null ? vb : value(f.invoke(va, vb));
    }

    Node restrict(long keep) {
        if (keep == bits) return this;
        if (keep == 0) return null;
        Object[] vs = vals == null ? null : new Object[Long.bitCount(keep)];
        int i = 0, j = 0;
        for (long r = bits; vs != null && r != 0; r &= r - 1, i++) {
            if ((keep & r & -r) != 0) vs[j++] = vals[i];
        }
        return new Node(null, base, 0, Long.bitCount(keep), keep, null, vs);
    }

    static Node range(Node n, long lo, long hi) {
        long first = n.base, last = n.base | ~above(n.shift);
        if (Long.compareUnsigned(last, lo) < 0 || Long.compareUnsigned(first, hi) > 0) {
            return null;
        }
        if (Long.compareUnsigned(first, lo) >= 0 && Long.compareUnsigned(last, hi) <= 0) {
            return n;
        }
        if (n.shift == 0) return n.restrict(n.bits & between(n, lo, hi));
        Node[] ks = new Node[n.kids.length];
        long bm = 0;
        int c = 0;
        for (long r = n.bits; r != 0; r &= r - 1) {
            Node k = range(n.kids[index(n.bits, r & -r)], lo, hi);
            if (k == null) continue;
            ks[c++] = k;
            bm |= r & -r;
        }
        return pack(n.base, n.shift, bm, ks, c, n, null);
    }

    static long between(Node leaf, long lo, long hi) {
        int from = Long.compareUnsigned(lo, leaf.base) <= 0 ? 0 : slot(lo, 0);
        int to = Long.compareUnsigned(hi, leaf.base | MASK) >= 0 ? MASK : slot(hi, 0);
        return (-1L << from) & (-1L >>> (63 - to));
    }

    static long first(Node n) {
        while (n.shift != 0) n = n.kids[0];
        return n.keyAt(n.bits & -n.bits);
    }

    static long last(Node n) {
        while (n.shift != 0) n = n.kids[n.kids.length - 1];
        return n.keyAt(Long.highestOneBit(n.bits));
    }

    static Node keys(Node n) {
        Node[] ks = n.kids == null ? null : new Node[n.kids.length];
        for (int i = 0; ks != null && i < ks.length; i++) ks[i] = keys(n.kids[i]);
        return new Node(null, n.base, n.shift, n.count, n.bits, ks, null);
    }

    static Object step(int mode, IFn f, Object acc, long k, Object v) {
        return switch (mode) {
            case KEYS -> f instanceof IFn.OLO p ? p.invokePrim(acc, k) : f.invoke(acc, k);
            case KV -> f instanceof IFn.OLOO p
                    ? p.invokePrim(acc, k, v)
                    : f.invoke(acc, k, v);
            default -> f.invoke(acc, MapEntry.create(k, v));
        };
    }

    static Object reduce(Node n, int mode, IFn f, Object acc) {
        if (n.shift != 0) {
            for (Node k : n.kids) {
                acc = reduce(k, mode, f, acc);
                if (RT.isReduced(acc)) return acc;
            }
            return acc;
        }
        int i = 0;
        for (long r = n.bits; r != 0; r &= r - 1, i++) {
            acc = step(mode, f, acc, n.keyAt(r & -r), n.vals == null ? null : n.vals[i]);
            if (RT.isReduced(acc)) return acc;
        }
        return acc;
    }

    static Object unreduced(Object acc) {
        return acc instanceof Reduced r ? r.deref() : acc;
    }

    static Object emit(IFn f, Object acc, long k, Object old, Object nu, boolean set) {
        if (set) {
            return f instanceof IFn.OLOO p
                    ? p.invokePrim(acc, k, nu != null)
                    : f.invoke(acc, k, nu != null);
        }
        return f instanceof IFn.OLOOO p
                ? p.invokePrim(acc, k, old, nu)
                : f.invoke(acc, k, old, nu);
    }

    static Object side(Node n, boolean old, IFn f, Object acc, boolean set) {
        if (n.shift != 0) {
            for (Node k : n.kids) {
                acc = side(k, old, f, acc, set);
                if (RT.isReduced(acc)) return acc;
            }
            return acc;
        }
        int i = 0;
        for (long r = n.bits; r != 0; r &= r - 1, i++) {
            Object v = n.vals == null ? Boolean.TRUE : n.vals[i];
            acc = emit(f, acc, n.keyAt(r & -r), old ? v : null, old ? null : v, set);
            if (RT.isReduced(acc)) return acc;
        }
        return acc;
    }

    static Object diff(Node a, Node b, IFn f, Object acc, boolean set) {
        if (a == b) return acc;
        if (a == null) return side(b, false, f, acc, set);
        if (b == null) return side(a, true, f, acc, set);
        Node top = a.shift >= b.shift ? a : b;
        if (apart(top, top == a ? b : a)) {
            boolean aFirst = Long.compareUnsigned(a.base, b.base) < 0;
            acc = diff(aFirst ? a : null, aFirst ? null : b, f, acc, set);
            return RT.isReduced(acc)
                    ? acc
                    : diff(aFirst ? null : a, aFirst ? b : null, f, acc, set);
        }
        if (top.shift == 0) return diffLeaves(a, b, f, acc, set);
        int s = top.shift;
        long as = slots(a, s), bs = slots(b, s);
        for (long r = as | bs; r != 0; r &= r - 1) {
            acc = diff(kid(a, s, as, r & -r), kid(b, s, bs, r & -r), f, acc, set);
            if (RT.isReduced(acc)) return acc;
        }
        return acc;
    }

    static Object diffLeaves(Node a, Node b, IFn f, Object acc, boolean set) {
        for (long r = a.bits | b.bits; r != 0; r &= r - 1) {
            long k = a.keyAt(r & -r), u = u(k);
            Object va = (a.bits & r & -r) == 0 ? null : a.val(u);
            Object vb = (b.bits & r & -r) == 0 ? null : b.val(u);
            if (va == vb) continue;
            acc = emit(f, acc, k, va, vb, set);
            if (RT.isReduced(acc)) return acc;
        }
        return acc;
    }

    /// Reducers' fork-join functions, as `PersistentHashMap.fold` takes
    /// them, so a fold runs on the reducers' pool.
    record Fork(IFn invoke, IFn task, IFn fork, IFn join) {}

    static Object fold(Node n, int leaf, IFn combinef, IFn reducef, int mode, Fork fj) {
        if (n == null) return combinef.invoke();
        return fj.invoke.invoke(new AFn() {
            @Override
            public Object invoke() {
                return part(n, leaf, combinef, reducef, mode, fj);
            }
        });
    }

    static Object part(Node n, int leaf, IFn combinef, IFn reducef, int mode, Fork fj) {
        if (n.count <= leaf || n.shift == 0) {
            return unreduced(reduce(n, mode, reducef, combinef.invoke()));
        }
        Object[] tasks = new Object[n.kids.length];
        for (int i = 1; i < tasks.length; i++) {
            Node k = n.kids[i];
            tasks[i] = fj.fork.invoke(fj.task.invoke(new AFn() {
                @Override
                public Object invoke() {
                    return part(k, leaf, combinef, reducef, mode, fj);
                }
            }));
        }
        Object acc = part(n.kids[0], leaf, combinef, reducef, mode, fj);
        for (int i = 1; i < tasks.length; i++) {
            acc = combinef.invoke(acc, fj.join.invoke(tasks[i]));
        }
        return acc;
    }

    static final class Walk implements Iterator<Object> {
        final ArrayDeque<Node> todo = new ArrayDeque<>();
        final int mode;
        final boolean reverse;
        Node leaf;
        long rest;
        int i;

        Walk(Node root, int mode, boolean reverse) {
            this.mode = mode;
            this.reverse = reverse;
            if (root != null) todo.push(root);
            advance();
        }

        void advance() {
            while (rest == 0 && !todo.isEmpty()) {
                Node n = todo.pop();
                if (n.shift == 0) {
                    leaf = n;
                    rest = n.bits;
                    i = reverse ? n.count - 1 : 0;
                } else if (reverse) {
                    for (Node k : n.kids) todo.push(k);
                } else {
                    for (int j = n.kids.length - 1; j >= 0; j--) todo.push(n.kids[j]);
                }
            }
        }

        @Override
        public boolean hasNext() {
            return rest != 0;
        }

        @Override
        public Object next() {
            if (rest == 0) throw new NoSuchElementException();
            long bit = reverse ? Long.highestOneBit(rest) : rest & -rest;
            rest &= ~bit;
            long k = leaf.keyAt(bit);
            Object v = leaf.vals == null ? null : leaf.vals[i];
            i += reverse ? -1 : 1;
            if (rest == 0) advance();
            return switch (mode) {
                case ENTRIES -> MapEntry.create(k, v);
                case VALS -> v;
                default -> k;
            };
        }
    }

    static Node fromSorted(long[] ks, Object[] vs) {
        Object e = new Object();
        Node n = null;
        for (int i = 0; i < ks.length; i++) {
            n = put(n, u(ks[i]), vs == null ? null : value(vs[i]), e);
        }
        return n;
    }

    static int check(Node n, int above, boolean map) {
        boolean placed = n.shift < above && n.shift % BITS == 0;
        need(n.bits != 0 && placed, "empty or misplaced node");
        need((n.base & ~above(n.shift)) == 0, "base has bits below the node");
        int slots = n.shift == 0 ? n.count : n.kids.length;
        need(n.count > 0 && Long.bitCount(n.bits) == slots, "slot count");
        return n.shift == 0 ? checkLeaf(n, map) : checkBranch(n, map);
    }

    static int checkLeaf(Node n, boolean map) {
        boolean vals = map ? n.vals != null && n.vals.length == n.count : n.vals == null;
        need(n.kids == null && vals, "leaf arrays");
        for (int i = 0; n.vals != null && i < n.count; i++) {
            need(n.vals[i] != null, "nil value");
        }
        return n.count;
    }

    static int checkBranch(Node n, boolean map) {
        need(n.vals == null && n.kids.length >= 2, "branch with fewer than two children");
        int c = 0;
        for (Node k : n.kids) {
            long bit = n.bit(k.base);
            boolean slotted = (n.bits & bit) != 0 && n.kids[index(n.bits, bit)] == k;
            need(n.covers(k.base) && slotted, "child out of its slot");
            c += check(k, n.shift, map);
        }
        need(c == n.count, "branch count");
        return c;
    }

    static void need(boolean ok, String what) {
        if (!ok) throw new IllegalStateException("Broken trie, " + what);
    }
}
