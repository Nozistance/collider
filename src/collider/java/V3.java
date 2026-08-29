package collider.java;

import clojure.lang.Counted;
import clojure.lang.IHashEq;
import clojure.lang.ILookup;
import clojure.lang.IPersistentCollection;
import clojure.lang.Indexed;
import clojure.lang.Murmur3;
import clojure.lang.RT;
import clojure.lang.Sequential;
import clojure.lang.Seqable;
import clojure.lang.ISeq;
import clojure.lang.Util;

import java.util.Iterator;
import java.util.NoSuchElementException;

public record V3(double x, double y, double z)
        implements Indexed, Counted, Sequential, Seqable, ILookup,
        IPersistentCollection, IHashEq, Iterable<Object> {
    private double at(int i) {
        return switch (i) {
            case 0 -> x;
            case 1 -> y;
            case 2 -> z;
            default -> throw new IndexOutOfBoundsException(String.valueOf(i));
        };
    }
    public Object nth(int i) {
        return at(i);
    }
    public Object nth(int i, Object notFound) {
        return i >= 0 && i < 3 ? at(i) : notFound;
    }
    public int count() {
        return 3;
    }
    public Object valAt(Object k) {
        return valAt(k, null);
    }
    public Object valAt(Object k, Object notFound) {
        return k instanceof Number ? nth(((Number) k).intValue(), notFound) : notFound;
    }
    public ISeq seq() {
        return RT.seq(new Object[]{x, y, z});
    }
    public Iterator<Object> iterator() {
        return new Iterator<>() {
            private int i = 0;
            public boolean hasNext() {
                return i < 3;
            }
            public Object next() {
                if (i >= 3) throw new NoSuchElementException();
                return at(i++);
            }
        };
    }
    public IPersistentCollection cons(Object o) {
        return RT.vector(x, y, z, o);
    }
    public IPersistentCollection empty() {
        return RT.vector();
    }
    public boolean equiv(Object o) {
        if (o instanceof V3) {
            V3 v = (V3) o;
            return x == v.x && y == v.y && z == v.z;
        }
        if (o instanceof Sequential) {
            return RT.count(o) == 3
                    && Util.equiv(x, RT.nth(o, 0))
                    && Util.equiv(y, RT.nth(o, 1))
                    && Util.equiv(z, RT.nth(o, 2));
        }
        return false;
    }
    public int hasheq() {
        return Murmur3.hashOrdered(this);
    }
    @Override
    public int hashCode() {
        int h = 1;
        h = 31 * h + Double.hashCode(x);
        h = 31 * h + Double.hashCode(y);
        h = 31 * h + Double.hashCode(z);
        return h;
    }
    @Override
    public boolean equals(Object o) {
        return equiv(o);
    }
    @Override
    public String toString() {
        return "[" + x + " " + y + " " + z + "]";
    }
}
