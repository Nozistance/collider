package collider.java;

import java.util.Arrays;

/// The open set of a path search: a binary heap of nodes, the node
/// of the lowest total score on top. Each node knows its place.
public final class PathHeap {

    private PathNode[] a = new PathNode[128];
    private int n;

    /// Returns true when the heap holds no node.
    public boolean isEmpty() {
        return n == 0;
    }

    private void put(int i, PathNode node) {
        a[i] = node;
        node.heapIdx = i;
    }

    private void up(int idx) {
        PathNode node = a[idx];
        double c = node.f;
        int i = idx;
        while (i > 0 && c < a[(i - 1) >> 1].f) {
            int p = (i - 1) >> 1;
            put(i, a[p]);
            i = p;
        }
        put(i, node);
    }

    private int lowerChild(int i) {
        int l = 2 * i + 1, r = l + 1;
        if (l >= n) return -1;
        if (r >= n) return l;
        return a[l].f < a[r].f ? l : r;
    }

    private void down(int idx) {
        PathNode node = a[idx];
        double c = node.f;
        int i = idx;
        for (int j = lowerChild(i); j != -1 && a[j].f < c;
             j = lowerChild(i)) {
            put(i, a[j]);
            i = j;
        }
        put(i, node);
    }

    /// Adds `node`, whose total score is set.
    public void insert(PathNode node) {
        if (n == a.length) a = Arrays.copyOf(a, 2 * n);
        put(n, node);
        n++;
        up(n - 1);
    }

    /// Removes and returns the node of the lowest total score.
    public PathNode pop() {
        PathNode top = a[0];
        int last = n - 1;
        put(0, a[last]);
        a[last] = null;
        n = last;
        if (last > 0) down(0);
        top.heapIdx = -1;
        return top;
    }

    /// Sets the total score of `node`, which the heap holds, to
    /// `cost` and moves the node to its new place.
    void changeCost(PathNode node, double cost) {
        double c = PathNode.fl(cost), old = node.f;
        node.f = c;
        if (c < old) {
            up(node.heapIdx);
        } else {
            down(node.heapIdx);
        }
    }
}
