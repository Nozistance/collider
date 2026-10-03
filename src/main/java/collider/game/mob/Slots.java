package collider.game.mob;

import java.util.Arrays;
import java.util.Comparator;

/// The place of each body of an island by its id.
public final class Slots {

    private final long[] ids;
    private final int[] at;

    private Slots(long[] ids, int[] at) {
        this.ids = ids;
        this.at = at;
    }

    /// Returns the places of the bodies `eids`, each at its index.
    public static Slots of(long[] eids) {
        int n = eids.length;
        boolean sorted = true;
        for (int i = 1; i < n && sorted; i++) {
            sorted = eids[i - 1] < eids[i];
        }
        int[] at = new int[n];
        for (int i = 0; i < n; i++) at[i] = i;
        if (sorted) return new Slots(eids, at);
        Integer[] order = new Integer[n];
        for (int i = 0; i < n; i++) order[i] = i;
        Arrays.sort(order, Comparator.comparingLong(i -> eids[i]));
        long[] ids = new long[n];
        for (int i = 0; i < n; i++) {
            ids[i] = eids[order[i]];
            at[i] = order[i];
        }
        return new Slots(ids, at);
    }

    /// Returns the index of body `eid` in `s`, or -1 for a body that
    /// is not in the island.
    public static long slot(Slots s, long eid) {
        int i = Arrays.binarySearch(s.ids, eid);
        return i < 0 ? -1 : s.at[i];
    }
}
