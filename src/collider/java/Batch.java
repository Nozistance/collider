package collider.java;

import java.util.Arrays;

/// The block edits of one section, gathered in order and applied
/// at once.
public final class Batch {

    private int[] idx = new int[8];
    private int[] states = new int[8];
    private int n;

    public void add(int i, int state) {
        if (n == idx.length) {
            idx = Arrays.copyOf(idx, 2 * n);
            states = Arrays.copyOf(states, 2 * n);
        }
        idx[n] = i;
        states[n] = state;
        n++;
    }

    public Section applyTo(Section s) {
        return s.apply(idx, states, n);
    }
}
