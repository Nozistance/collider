package collider.game.mob;

import collider.world.LongIntMap;
import java.util.Arrays;

/// The groups of bodies that one tick of movement cannot bring
/// together. Bodies share a group when their cells of the push grid
/// touch, directly or through a chain of touching cells.
public final class Islands {

    static long key(long cx, long cz) {
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

    private static int[] linked(long[] cells, int m, LongIntMap at) {
        int[] up = new int[m];
        for (int i = 0; i < m; i++) up[i] = i;
        for (int i = 0; i < m; i++) {
            long cx = (int) (cells[i] >> 32), cz = (int) cells[i];
            for (int dx = -1; dx <= 1; dx++) {
                for (int dz = -1; dz <= 1; dz++) {
                    int j = at.get(key(cx + dx, cz + dz));
                    if (j > i) join(up, i, j);
                }
            }
        }
        return up;
    }

    private static int[] byId(long[] eids) {
        int n = eids.length;
        int[] out = new int[n];
        boolean sorted = true;
        for (int i = 0; i < n; i++) {
            out[i] = i;
            sorted &= i == 0 || eids[i - 1] < eids[i];
        }
        if (sorted) return out;
        LongIntMap at = new LongIntMap(n);
        for (int i = 0; i < n; i++) at.put(eids[i], i);
        long[] s = eids.clone();
        Arrays.sort(s);
        for (int k = 0; k < n; k++) out[k] = at.get(s[k]);
        return out;
    }

    /// Returns the indices of the bodies with ids `eids` in cells
    /// `keys`, in groups. Each group is in the order of its ids and
    /// the groups are in the order of their first id.
    public static int[][] of(long[] eids, long[] keys) {
        int n = eids.length;
        LongIntMap at = new LongIntMap(n);
        long[] cells = new long[n];
        int[] cell = new int[n];
        int m = 0;
        for (int i = 0; i < n; i++) {
            int c = at.get(keys[i]);
            if (c < 0) {
                c = m++;
                at.put(keys[i], c);
                cells[c] = keys[i];
            }
            cell[i] = c;
        }
        int[] up = linked(cells, m, at);
        int[] order = byId(eids);
        int[] group = new int[m];
        Arrays.fill(group, -1);
        int[] sizes = new int[n];
        int[] of = new int[n];
        int g = 0;
        for (int k = 0; k < n; k++) {
            int r = root(up, cell[order[k]]);
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
