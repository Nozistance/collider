package collider.world.feature;

import clojure.lang.IPersistentVector;
import clojure.lang.LazilyPersistentVector;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;

public final class Cells {
    private record Cell(long x, long y, long z) {
        @Override
        public int hashCode() {
            return ((int) y + (int) z * 31) * 31 + (int) x;
        }

        IPersistentVector vec() {
            return LazilyPersistentVector.createOwning(x, y, z);
        }
    }

    private final HashSet<Cell> set = new HashSet<>();

    private static Cell of(Object p) {
        IPersistentVector v = (IPersistentVector) p;
        return new Cell(((Number) v.nth(0)).longValue(),
                        ((Number) v.nth(1)).longValue(),
                        ((Number) v.nth(2)).longValue());
    }

    public static Cells of(Iterable<?> ps) {
        Cells c = new Cells();
        for (Object p : ps) c.set.add(of(p));
        return c;
    }

    public boolean add(Object p) {
        return set.add(of(p));
    }

    public boolean has(Object p) {
        return set.contains(of(p));
    }

    public boolean isEmpty() {
        return set.isEmpty();
    }

    public Object poll() {
        Iterator<Cell> it = set.iterator();
        Cell c = it.next();
        it.remove();
        return c.vec();
    }

    public Object order() {
        ArrayList<Object> out = new ArrayList<>(set.size());
        for (Cell c : set) out.add(c.vec());
        return LazilyPersistentVector.create(out);
    }
}
