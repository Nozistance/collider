package collider.world.space;

import collider.world.Chunk;
import collider.world.ChunkIndex;
import java.util.HashMap;

/// The numeric core of the ground path search. `shapes` holds the
/// collision boxes of each block state, six doubles each, in blocks.
public final class Path {

    private static final double FUDGING = 1.5;

    private static final double EPS = 1.0E-7;

    private static long key(long x, long y, long z) {
        return (int) ((y & 0xFF) | ((x & 32767) << 8)
                      | ((z & 32767) << 24)
                      | (x < 0 ? Integer.MIN_VALUE : 0)
                      | (z < 0 ? 32768 : 0));
    }

    /// Returns the node of the cell `x`, `y`, `z` in `nodes` and adds
    /// it when absent. Cells that share a key share a node.
    public static PathNode node(HashMap<Long, PathNode> nodes, long x,
            long y, long z) {
        return nodes.computeIfAbsent(key(x, y, z),
                                     k -> new PathNode(x, y, z));
    }

    private static long cell(long x, long y, long z) {
        return ((x & 0x3FFFFFFL) << 38) | ((z & 0x3FFFFFFL) << 12)
               | (y & 0xFFFL);
    }

    /// Returns the path type one search gave the cell `x`, `y`, `z`,
    /// null when it has not typed that cell yet.
    public static Object cachedType(HashMap<Long, Object> types, long x,
            long y, long z) {
        return types.get(cell(x, y, z));
    }

    /// Keeps `t` as the path type of the cell `x`, `y`, `z` for the
    /// rest of one search and returns it.
    public static Object cacheType(HashMap<Long, Object> types, long x,
            long y, long z, Object t) {
        types.put(cell(x, y, z), t);
        return t;
    }

    /// Returns what the first cell around `x`, `y`, `z` forces upon
    /// it through `forced`, the forced type of each block state, in
    /// the order WalkNodeEvaluator scans them; null when none does.
    public static Object forced(ChunkIndex chunks, Object[] forced,
            long x, long y, long z) {
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dz = -1; dz <= 1; dz++) {
                    if (dx == 0 && dz == 0) continue;
                    int st = Chunk.blockAt(chunks, (int) (x + dx),
                                           (int) (y + dy),
                                           (int) (z + dz));
                    if (st < 0 || st >= forced.length) continue;
                    Object f = forced[st];
                    if (f != null) return f;
                }
            }
        }
        return null;
    }

    /// Returns the top of the collision shape of the block state
    /// `st`, in blocks, 0 when it has no shape.
    public static double shapeTop(Object[] shapes, int st) {
        double[] s = (double[]) shapes[st];
        if (s.length == 0) return 0.0;
        double top = s[4];
        for (int k = 10; k < s.length; k += 6) {
            top = Math.max(top, s[k]);
        }
        return top;
    }

    private static boolean cellHits(Object[] shapes, int st, double[] b,
            long x, long y, long z) {
        double[] s = (double[]) shapes[st];
        for (int k = 0; k < s.length; k += 6) {
            if (x + s[k + 3] > b[0] && x + s[k] < b[3]
                && y + s[k + 4] > b[1] && y + s[k + 1] < b[4]
                && z + s[k + 5] > b[2] && z + s[k + 2] < b[5]) {
                return true;
            }
        }
        return false;
    }

    /// Returns true when the box `b` meets a collision shape of a
    /// block in `chunks`. `b` holds min x, y, z and max x, y, z.
    public static boolean collides(ChunkIndex chunks, Object[] shapes,
            double[] b) {
        long x1 = (long) Math.floor(b[3] + EPS);
        long y1 = (long) Math.floor(b[4] + EPS);
        long z1 = (long) Math.floor(b[5] + EPS);
        for (long x = (long) Math.floor(b[0] - EPS); x <= x1; x++) {
            for (long y = (long) Math.floor(b[1] - EPS); y <= y1; y++) {
                for (long z = (long) Math.floor(b[2] - EPS); z <= z1;
                     z++) {
                    int st = Chunk.blockAt(chunks, (int) x, (int) y,
                                           (int) z);
                    if (cellHits(shapes, st, b, x, y, z)) return true;
                }
            }
        }
        return false;
    }

    /// Returns true when a mob box of `width` and `height` standing
    /// at `px`, `py`, `pz` slides to the node `n` without meeting a
    /// collision shape in `chunks`.
    public static boolean canReach(ChunkIndex chunks, Object[] shapes,
            double px, double py, double pz, double width,
            double height, PathNode n) {
        double dx = (n.x - px) + width / 2.0;
        double dy = (n.y - py) + height / 2.0;
        double dz = (n.z - pz) + width / 2.0;
        double size = (width + height + width) / 3.0;
        double len = Math.sqrt(dx * dx + dy * dy + dz * dz);
        long steps = (long) Math.ceil(len / size);
        double k = (float) (1.0 / steps);
        double w = width / 2.0;
        double[] b = {px - w, py, pz - w, px + w, py + height, pz + w};
        double sx = dx * k, sy = dy * k, sz = dz * k;
        for (long i = 1; i <= steps; i++) {
            b[0] += sx;
            b[1] += sy;
            b[2] += sz;
            b[3] += sx;
            b[4] += sy;
            b[5] += sz;
            if (collides(chunks, shapes, b)) return false;
        }
        return true;
    }

    /// Returns the smallest straight distance from `n` to the
    /// `targets`, and lets each target keep `n` when `n` is the
    /// closest node to it so far.
    public static double bestH(PathTarget[] targets, PathNode n) {
        double best = Float.MAX_VALUE;
        for (PathTarget t : targets) {
            double h = n.distTo(t.x, t.y, t.z);
            t.offer(h, n);
            best = Math.min(h, best);
        }
        return best;
    }

    /// Starts a search from the node `from` into `heap`.
    public static void start(PathHeap heap, PathTarget[] targets,
            PathNode from) {
        from.g = 0.0;
        from.h = PathNode.fl(bestH(targets, from));
        from.f = from.h;
        heap.insert(from);
    }

    /// Lets the search step from `cur` onto its neighbour `n`. The
    /// step counts when the walked length stays under `maxlen` and
    /// it is the cheapest way to `n` so far.
    public static void relax(PathHeap heap, PathTarget[] targets,
            double maxlen, PathNode cur, PathNode n) {
        double d = cur.distTo(n.x, n.y, n.z);
        double w = PathNode.fl(cur.walked + d);
        double g = PathNode.fl(PathNode.fl(cur.g + d) + n.malus());
        n.walked = w;
        boolean open = n.heapIdx >= 0;
        if (w < maxlen && (!open || g < n.g)) {
            n.came = cur;
            n.g = g;
            n.h = PathNode.fl(bestH(targets, n) * FUDGING);
            if (open) {
                heap.changeCost(n, n.g + n.h);
            } else {
                n.f = PathNode.fl(n.g + n.h);
                heap.insert(n);
            }
        }
    }
}
