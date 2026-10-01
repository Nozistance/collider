package collider.game.mob;

import java.util.Arrays;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;

/// The groups of bodies that one tick of movement cannot bring
/// together. Bodies share a group when their cells of the push grid
/// touch, directly or through a chain of touching cells.
public final class Islands {

    private Islands() {
    }

    /// Returns the number of threads that the folds started from the
    /// calling thread share, where a thread outside any pool folds in
    /// `pool`.
    public static int threads(ForkJoinPool pool) {
        ForkJoinPool p = ForkJoinTask.getPool();
        return (p == null ? pool : p).getParallelism();
    }

    private static long key(long cx, long cz) {
        return ((cx & 0xFFFFFFFFL) << 32) | (cz & 0xFFFFFFFFL);
    }

    private static int root(int[] up, int i) {
        while (up[i] != i) {
            up[i] = up[up[i]];
            i = up[i];
        }
        return i;
    }

    private static void join(int[] up, int a, int b) {
        int ra = root(up, a), rb = root(up, b);
        if (ra != rb) {
            up[Math.max(ra, rb)] = Math.min(ra, rb);
        }
    }

    private static int[] table(long[] cells) {
        int size = Integer.highestOneBit(Math.max(4, cells.length * 4));
        int[] t = new int[size];
        Arrays.fill(t, -1);
        for (int i = 0; i < cells.length; i++) {
            int h = slot(cells[i], size);
            while (t[h] >= 0) h = (h + 1) & (size - 1);
            t[h] = i;
        }
        return t;
    }

    private static int slot(long k, int size) {
        long h = k * 0x9E3779B97F4A7C15L;
        return (int) (h >>> 40) & (size - 1);
    }

    private static int find(int[] t, long[] cells, long k) {
        int size = t.length;
        for (int h = slot(k, size); t[h] >= 0; h = (h + 1) & (size - 1)) {
            if (cells[t[h]] == k) return t[h];
        }
        return -1;
    }

    private static int[] linked(long[] cells, int[] t) {
        int m = cells.length;
        int[] up = new int[m];
        for (int i = 0; i < m; i++) up[i] = i;
        for (int i = 0; i < m; i++) {
            long cx = (int) (cells[i] >> 32), cz = (int) cells[i];
            for (int dx = -1; dx <= 1; dx++) {
                for (int dz = -1; dz <= 1; dz++) {
                    int j = find(t, cells, key(cx + dx, cz + dz));
                    if (j > i) join(up, i, j);
                }
            }
        }
        return up;
    }

    private static long[] distinct(long[] keys) {
        long[] s = keys.clone();
        Arrays.sort(s);
        int m = 0;
        for (int i = 0; i < s.length; i++) {
            if (m == 0 || s[m - 1] != s[i]) s[m++] = s[i];
        }
        return Arrays.copyOf(s, m);
    }

    private static int[] byId(long[] eids) {
        int n = eids.length;
        Integer[] order = new Integer[n];
        boolean sorted = true;
        for (int i = 0; i < n; i++) {
            order[i] = i;
            sorted &= i == 0 || eids[i - 1] < eids[i];
        }
        if (!sorted) {
            Arrays.sort(order, (a, b) -> Long.compare(eids[a], eids[b]));
        }
        int[] out = new int[n];
        for (int i = 0; i < n; i++) out[i] = order[i];
        return out;
    }

    /// Returns the indices of the bodies with ids `eids` in cells
    /// `keys`, in groups. Each group is in the order of its ids and
    /// the groups are in the order of their first id.
    public static int[][] of(long[] eids, long[] keys) {
        int n = eids.length;
        long[] cells = distinct(keys);
        int[] t = table(cells);
        int[] up = linked(cells, t);
        int[] order = byId(eids);
        int[] group = new int[cells.length];
        Arrays.fill(group, -1);
        int[] sizes = new int[n];
        int[] of = new int[n];
        int g = 0;
        for (int k = 0; k < n; k++) {
            int i = order[k];
            int r = root(up, find(t, cells, keys[i]));
            if (group[r] < 0) group[r] = g++;
            of[k] = group[r];
            sizes[of[k]]++;
        }
        int[][] out = new int[g][];
        for (int j = 0; j < g; j++) out[j] = new int[sizes[j]];
        int[] fill = new int[g];
        for (int k = 0; k < n; k++) out[of[k]][fill[of[k]]++] = order[k];
        return out;
    }
}
