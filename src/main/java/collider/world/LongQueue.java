package collider.world;

import java.util.ArrayDeque;

/// Longs taken in the order they were added, for one pass, never kept
/// or shared. The longs live in blocks that double in size up to a
/// limit, and a block taken to its end holds the next ones added, so
/// the queue never copies and holds about as much as it has waiting at
/// most.
/// In Java because it is a mutable structure on primitive arrays.
public final class LongQueue {

    private static final int SIZE = 1024;

    private final ArrayDeque<long[]> next = new ArrayDeque<>();
    private long[] out = new long[64];
    private long[] in = out;
    private long[] spare;
    private int head;
    private int tail;

    public boolean isEmpty() {
        return out == in && head == tail;
    }

    public void add(long e) {
        if (tail == in.length) {
            int size = Math.min(SIZE, 2 * in.length);
            in = spare != null && spare.length == size ? spare : new long[size];
            spare = null;
            next.addLast(in);
            tail = 0;
        }
        in[tail++] = e;
    }

    /// Takes the oldest long, which must be there.
    public long poll() {
        if (head == out.length) {
            spare = out;
            out = next.removeFirst();
            head = 0;
        }
        return out[head++];
    }
}
