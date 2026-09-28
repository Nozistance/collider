package collider.world;

import java.util.Arrays;

/// The block edits of one section, applied at once in order.
public final class Batch {

    private int[] idx = new int[8];
    private int[] states = new int[8];
    private int n;

    /// Adds the edit that sets block `i` to `state`.
    public void add(int i, int state) {
        if (n == idx.length) {
            idx = Arrays.copyOf(idx, 2 * n);
            states = Arrays.copyOf(states, 2 * n);
        }
        idx[n] = i;
        states[n] = state;
        n++;
    }

    /// Returns `s` with the edits applied in order.
    public Section applyTo(Section s) {
        return s.apply(idx, states, n);
    }
}
