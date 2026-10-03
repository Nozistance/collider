package collider.world;

import java.util.Arrays;

/// Longs taken in the order they were added, for one pass, never kept
/// or shared.
public final class LongQueue {

    private long[] items = new long[64];
    private int head;
    private int tail;

    public boolean isEmpty() {
        return head == tail;
    }

    public void add(long e) {
        if (tail == items.length) {
            items = Arrays.copyOfRange(items, head, head + 2 * items.length);
            tail -= head;
            head = 0;
        }
        items[tail++] = e;
    }

    /// Takes the oldest long, which must be there.
    public long poll() {
        return items[head++];
    }
}
