package collider.game.block;

import java.util.HashMap;

/// The order a loaded chunk hands its block entities to the level:
/// the pending map of the saved chunk copied into the pending map of
/// the level chunk, both keyed by BlockPos.
public final class HashOrder {

    private HashOrder() {
    }

    private record Pos(int x, int y, int z) {
        @Override
        public int hashCode() {
            return (y + z * 31) * 31 + x;
        }
    }

    /// Returns the indices of the positions `xyz`, three ints each,
    /// in the order the level chunk iterates them.
    public static int[] of(int[] xyz) {
        int n = xyz.length / 3;
        HashMap<Pos, Integer> saved = new HashMap<>();
        for (int i = 0; i < n; i++) {
            int j = 3 * i;
            saved.put(new Pos(xyz[j], xyz[j + 1], xyz[j + 2]), i);
        }
        HashMap<Pos, Integer> pending = new HashMap<>();
        pending.putAll(saved);
        int[] out = new int[n];
        int k = 0;
        for (Integer i : pending.values()) out[k++] = i;
        return out;
    }
}
