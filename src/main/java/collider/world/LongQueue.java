package collider.world;

import java.util.Arrays;

/// Longs taken in the order they were added, for one pass, never kept
/// or shared. The longs not yet taken move to the front before the
/// queue grows, so a queue drained as it fills stays small.
public final class LongQueue {

    private long[] items = new long[64];
    private int head;
    private int tail;

    public boolean isEmpty() {
        return head == tail;
    }

    public void add(long e) {
        if (tail == items.length) {
            int live = tail - head;
            if (2 * live <= items.length) {
                System.arraycopy(items, head, items, 0, live);
            } else {
                items = Arrays.copyOfRange(items, head, head + 2 * items.length);
            }
            tail = live;
            head = 0;
        }
        items[tail++] = e;
    }

    /// Takes the oldest long, which must be there.
    public long poll() {
        return items[head++];
    }
}
