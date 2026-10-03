package collider.world.space;

import collider.Cell;
import collider.world.Chunk;
import collider.world.ChunkIndex;
import collider.world.Collision;
import collider.world.Scratch;

/// The numeric core of the ground path search of a mob.
public final class Path {

    private static final double FUDGING = 1.5;

    private static final double EPS = 1.0E-7;

    static final int BLOCKED = 0,
            OPEN = 1,
            WALKABLE = 2,
            WALKABLE_DOOR = 3,
            TRAPDOOR = 4,
            POWDER_SNOW = 5,
            ON_TOP_OF_POWDER_SNOW = 6,
            FENCE = 7,
            LAVA = 8,
            WATER = 9,
            RAIL = 11,
            UNPASSABLE_RAIL = 12,
            FIRE = 14,
            DAMAGING = 16,
            DOOR_OPEN = 17,
            DOOR_WOOD_CLOSED = 18,
            DOOR_IRON_CLOSED = 19,
            STICKY_HONEY = 22,
            DAMAGE_CAUTIOUS = 24,
            ON_TOP_OF_TRAPDOOR = 25,
            BIG_MOBS_CLOSE_TO_DANGER = 26;

    private static final int[][] DIRS = {{0, 1}, {-1, 0}, {0, -1}, {1, 0}};

    private static final int[] HORIZONTAL = {2, 3, 0, 1};

    private static final int[] CLOCKWISE = {1, 2, 3, 0};

    private final ChunkIndex chunks;
    private final int[] types;
    private final int[] forced;
    private final boolean[] water;
    private final Object[] shapes;
    private final byte[] kinds;
    private final int ctx;
    private final double[] malus;
    private final double[] baseMalus;
    private final long minY;
    private final double px, py, pz, width, height, upStep;
    private final long maxFall, mx, my, mz, bbW, bbH;
    private final boolean floats, openDoors, passDoors, overFences;
    private final Scratch<PathNode> nodes = new Scratch<>(256);
    private final Scratch<Integer> typed = new Scratch<>(256);

    /// Makes one search for `mob` over `chunks` from `minY` up.
    /// `types`, `forced` and `water` hold the path type, the type
    /// forced upon neighbours (-1 for none) and whether it is water,
    /// for each block state. `malus` holds the malus of the mob and
    /// `baseMalus` the default malus, for each path type.
    public Path(
            ChunkIndex chunks,
            long minY,
            int[] types,
            int[] forced,
            boolean[] water,
            Object[] shapes,
            byte[] kinds,
            double[] malus,
            double[] baseMalus,
            PathMob mob
    ) {
        this.chunks = chunks;
        this.minY = minY;
        this.types = types;
        this.forced = forced;
        this.water = water;
        this.shapes = shapes;
        this.kinds = kinds;
        this.ctx = mob.ctx();
        this.malus = malus;
        this.baseMalus = baseMalus;
        px = mob.x();
        py = mob.y();
        pz = mob.z();
        width = mob.width();
        height = mob.height();
        upStep = mob.upStep();
        maxFall = mob.maxFall();
        floats = mob.floats();
        openDoors = mob.openDoors();
        passDoors = mob.passDoors();
        overFences = mob.overFences();
        mx = (long) Math.floor(px);
        my = (long) Math.floor(py);
        mz = (long) Math.floor(pz);
        bbW = (long) Math.floor(width + 1.0);
        bbH = (long) Math.floor(height + 1.0);
    }

    private static long key(long x, long y, long z) {
        return (int) ((y & 0xFF)
                | ((x & 32767) << 8)
                | ((z & 32767) << 24)
                | (x < 0 ? Integer.MIN_VALUE : 0)
                | (z < 0 ? 32768 : 0));
    }

    /// Returns the node of the cell `x`, `y`, `z` of search `p` and
    /// adds it when absent. Cells that share a key share a node.
    public static PathNode node(Path p, long x, long y, long z) {
        long k = key(x, y, z);
        PathNode n = p.nodes.get(k);
        if (n == null) {
            n = new PathNode(x, y, z);
            p.nodes.put(k, n);
        }
        return n;
    }

    private static int typeAt(ChunkIndex chunks, int[] types, long x, long y, long z) {
        int st = Chunk.blockAt(chunks, (int) x, (int) y, (int) z);
        return st < types.length ? types[st] : OPEN;
    }

    private static int floorType(
            ChunkIndex chunks,
            int[] types,
            int[] forced,
            long x,
            long y,
            long z
    ) {
        return switch (typeAt(chunks, types, x, y - 1, z)) {
            case OPEN, WATER, LAVA, WALKABLE -> OPEN;
            case FIRE -> FIRE;
            case DAMAGING -> DAMAGING;
            case STICKY_HONEY -> STICKY_HONEY;
            case POWDER_SNOW -> ON_TOP_OF_POWDER_SNOW;
            case DAMAGE_CAUTIOUS -> DAMAGE_CAUTIOUS;
            case TRAPDOOR -> ON_TOP_OF_TRAPDOOR;
            default -> {
                int f = forced(chunks, forced, x, y, z);
                yield f >= 0 ? f : WALKABLE;
            }
        };
    }

    /// Returns the path type of the cell `x`, `y`, `z` for a mob one
    /// cell tall, in a level from `lo` up.
    public static int staticType(
            ChunkIndex chunks,
            int[] types,
            int[] forced,
            long lo,
            long x,
            long y,
            long z
    ) {
        int t = typeAt(chunks, types, x, y, z);
        return t == OPEN && y >= lo + 1 ? floorType(chunks, types, forced, x, y, z) : t;
    }

    private int staticAt(long x, long y, long z) {
        return staticType(chunks, types, forced, minY, x, y, z);
    }

    private int bbType(long x, long y, long z) {
        int t = staticAt(x, y, z);
        if (t == DOOR_WOOD_CLOSED && openDoors && passDoors) {
            return WALKABLE_DOOR;
        }
        if (t == DOOR_OPEN && !passDoors) return BLOCKED;
        if (t == RAIL
                && staticAt(mx, my, mz) != RAIL
                && staticAt(mx, my - 1, mz) != RAIL) {
            return UNPASSABLE_RAIL;
        }
        return t;
    }

    private int typedForMob(long x, long y, long z) {
        int set = 0;
        for (long i = 0, n = bbW * bbW * bbH; i < n; i++) {
            long r = i / bbW;
            set |= 1 << bbType(x + r / bbH, y + r % bbH, z + i % bbW);
        }
        if (Integer.bitCount(set) == 1) {
            return Integer.numberOfTrailingZeros(set);
        }
        if ((set & (1 << FENCE)) != 0) return FENCE;
        if ((set & (1 << UNPASSABLE_RAIL)) != 0) return UNPASSABLE_RAIL;
        int bt = BLOCKED;
        double bm = malus[BLOCKED];
        for (int s = set; s != 0; s &= s - 1) {
            int t = Integer.numberOfTrailingZeros(s);
            double m = malus[t];
            if (m < 0.0) return t;
            if (m >= bm) {
                bt = t;
                bm = m;
            }
        }
        int cur = staticAt(x, y, z);
        if (bbW > 1) {
            boolean near = malus[cur] < bm && malus[BIG_MOBS_CLOSE_TO_DANGER] < bm;
            return near ? BIG_MOBS_CLOSE_TO_DANGER : bt;
        }
        return cur == OPEN && bt != OPEN && bm == 0.0 ? OPEN : bt;
    }

    /// Returns the path type of the cell `x`, `y`, `z` for the mob of
    /// search `p`. One search types each cell once.
    public static int typeOf(Path p, long x, long y, long z) {
        long k = Cell.pack(x, y, z);
        Integer t = p.typed.get(k);
        if (t != null) return t;
        int v = p.typedForMob(x, y, z);
        p.typed.put(k, v);
        return v;
    }

    private double floorLevel(long x, long y, long z) {
        if (floats) {
            int st = Chunk.blockAt(chunks, (int) x, (int) y, (int) z);
            if (st < water.length && water[st]) return y + 0.5;
        }
        int below = Chunk.blockAt(chunks, (int) x, (int) (y - 1), (int) z);
        return (y - 1) + shapeTop(shapes, below);
    }

    private PathNode withCost(long x, long y, long z, int t, double m) {
        PathNode n = node(this, x, y, z);
        n.kind = t;
        n.setMalus(Math.max(n.malus(), m));
        return n;
    }

    private PathNode blocked(long x, long y, long z) {
        PathNode n = node(this, x, y, z);
        n.kind = BLOCKED;
        n.setMalus(-1.0);
        return n;
    }

    private PathNode closedAt(long x, long y, long z, int t) {
        PathNode n = node(this, x, y, z);
        n.close();
        n.kind = t;
        n.setMalus(baseMalus[t]);
        return n;
    }

    private static boolean partial(int t) {
        return t == FENCE || t == DOOR_WOOD_CLOSED || t == DOOR_IRON_CLOSED;
    }

    private PathNode groundBelow(long x, long y, long z) {
        for (long cy = y - 1; ; cy--) {
            if (cy < minY) return blocked(x, y, z);
            if (y - cy > maxFall) return blocked(x, cy, z);
            int t = typeOf(this, x, cy, z);
            double m = malus[t];
            if (t == OPEN) continue;
            return m >= 0.0 ? withCost(x, cy, z, t, m) : blocked(x, cy, z);
        }
    }

    private PathNode nonWaterBelow(long x, long y, long z, PathNode best) {
        for (long cy = y - 1; cy > minY; cy--) {
            int t = typeOf(this, x, cy, z);
            if (t != WATER) return best;
            best = withCost(x, cy, z, t, malus[t]);
        }
        return best;
    }

    private PathNode jumpOn(
            long x,
            long y,
            long z,
            long jump,
            double nh,
            int dir,
            int cur
    ) {
        PathNode above = accepted(x, y + 1, z, jump - 1, nh, dir, cur);
        if (above == null) return null;
        if (width >= 1.0) return above;
        if (above.kind != OPEN && above.kind != WALKABLE) return above;
        long cx = x - DIRS[dir][0], cz = z - DIRS[dir][1];
        double hw = width / 2.0;
        double[] b = {
            cx + 0.5 - hw,
            floorLevel(cx, y + 1, cz) + 0.001,
            cz + 0.5 - hw,
            cx + 0.5 + hw,
            height + floorLevel(above.x, above.y, above.z) - 0.002,
            cz + 0.5 + hw
        };
        return collides(b) ? null : above;
    }

    private PathNode accepted(
            long x,
            long y,
            long z,
            long jump,
            double nh,
            int dir,
            int cur
    ) {
        if (floorLevel(x, y, z) - nh > Math.max(1.125, upStep)) {
            return null;
        }
        int t = typeOf(this, x, y, z);
        double m = malus[t];
        PathNode best = m >= 0.0 ? withCost(x, y, z, t, m) : null;
        if (partial(cur) && best != null && best.malus() >= 0.0 && !canReach(best)) {
            best = null;
        }
        if (t == WALKABLE) return best;
        if ((best == null || best.malus() < 0.0)
                && jump > 0
                && (t != FENCE || overFences)
                && t != UNPASSABLE_RAIL
                && t != TRAPDOOR
                && t != POWDER_SNOW) {
            return jumpOn(x, y, z, jump, nh, dir, cur);
        }
        if (t == WATER && !floats) return nonWaterBelow(x, y, z, best);
        if (t == OPEN) return groundBelow(x, y, z);
        if (partial(t) && best == null) return closedAt(x, y, z, t);
        return best;
    }

    private static boolean diagonalOk(
            double width,
            PathNode pos,
            PathNode ew,
            PathNode ns
    ) {
        if (ns == null || ew == null || ns.y > pos.y || ew.y > pos.y) {
            return false;
        }
        if (ew.kind == WALKABLE_DOOR || ns.kind == WALKABLE_DOOR) {
            return false;
        }
        if (width > 1.0 && (ew.malus() > 0.0 || ns.malus() > 0.0)) {
            return false;
        }
        boolean gap = ns.kind == FENCE && ew.kind == FENCE && width < 0.5;
        return (ns.y < pos.y || ns.malus() >= 0.0 || gap)
                && (ew.y < pos.y || ew.malus() >= 0.0 || gap);
    }

    private int neighbors(PathNode pos, PathNode[] out) {
        int cur = typeOf(this, pos.x, pos.y, pos.z);
        int above = typeOf(this, pos.x, pos.y + 1, pos.z);
        long js = malus[above] >= 0.0 && cur != STICKY_HONEY
                ? (long) Math.floor(Math.max(1.0, upStep))
                : 0;
        double ph = floorLevel(pos.x, pos.y, pos.z);
        PathNode[] side = new PathNode[4];
        for (int d : HORIZONTAL) {
            long x = pos.x + DIRS[d][0], z = pos.z + DIRS[d][1];
            side[d] = accepted(x, pos.y, z, js, ph, d, cur);
        }
        int k = 0;
        for (int d : HORIZONTAL) {
            PathNode n = side[d];
            if (n != null && !n.closed() && (n.malus() >= 0.0 || pos.malus() < 0.0)) {
                out[k++] = n;
            }
        }
        for (int d : HORIZONTAL) {
            int cw = CLOCKWISE[d];
            if (!diagonalOk(width, pos, side[d], side[cw])) continue;
            long x = pos.x + DIRS[d][0] + DIRS[cw][0];
            long z = pos.z + DIRS[d][1] + DIRS[cw][1];
            PathNode n = accepted(x, pos.y, z, js, ph, d, cur);
            if (n != null && !n.closed() && n.kind != WALKABLE_DOOR && n.malus() >= 0.0) {
                out[k++] = n;
            }
        }
        return k;
    }

    /// Returns the node search `p` starts from, at the cell `x`, `y`,
    /// `z`, typed for its mob.
    public static PathNode startAt(Path p, long x, long y, long z) {
        PathNode n = node(p, x, y, z);
        n.kind = typeOf(p, x, y, z);
        n.setMalus(p.malus[n.kind]);
        return n;
    }

    /// Returns the nodes of the path to the node closest to `t`,
    /// first to last.
    public static PathNode[] route(PathTarget t) {
        int k = 0;
        for (PathNode n = t.node(); n != null; n = n.came) k++;
        PathNode[] r = new PathNode[k];
        for (PathNode n = t.node(); n != null; n = n.came) r[--k] = n;
        return r;
    }

    public static long[] goal(PathTarget t) {
        return new long[] {t.x, t.y, t.z};
    }

    /// Returns how far the node closest to `t` stays from it, the
    /// largest float when no node came near.
    public static double gap(PathTarget t) {
        PathNode n = t.node();
        return n == null ? Float.MAX_VALUE : n.manhattan(t.x, t.y, t.z);
    }

    /// Runs search `p` from `from` towards `targets` and returns the
    /// targets it reached within `reach`, null when it reached none.
    /// It visits fewer than `maxv` nodes and walks no further than
    /// `maxlen` from `from`.
    public static PathTarget[] run(
            Path p,
            PathNode from,
            PathTarget[] targets,
            double maxlen,
            long reach,
            long maxv
    ) {
        PathHeap heap = new PathHeap();
        start(heap, targets, from);
        PathNode[] out = new PathNode[8];
        for (long c = 0; !heap.isEmpty() && c + 1 < maxv; c++) {
            PathNode cur = heap.pop();
            cur.close();
            int hits = 0;
            for (PathTarget t : targets) {
                if (cur.manhattan(t.x, t.y, t.z) <= reach) hits++;
            }
            if (hits > 0) {
                PathTarget[] r = new PathTarget[hits];
                int i = 0;
                for (PathTarget t : targets) {
                    if (cur.manhattan(t.x, t.y, t.z) <= reach) r[i++] = t;
                }
                return r;
            }
            if (cur.distTo(from.x, from.y, from.z) < maxlen) {
                int k = p.neighbors(cur, out);
                for (int i = 0; i < k; i++) {
                    relax(heap, targets, maxlen, cur, out[i]);
                }
            }
        }
        return null;
    }

    /// Returns what the first cell around `x`, `y`, `z` forces upon
    /// it through `forced`, the forced type of each block state, in
    /// the order the game scans them, or -1 when none does.
    public static int forced(ChunkIndex chunks, int[] forced, long x, long y, long z) {
        for (int dx = -1; dx <= 1; dx++) {
            for (int dy = -1; dy <= 1; dy++) {
                for (int dz = -1; dz <= 1; dz++) {
                    if (dx == 0 && dz == 0) continue;
                    int st = Chunk.blockAt(
                            chunks,
                            (int) (x + dx),
                            (int) (y + dy),
                            (int) (z + dz)
                    );
                    if (st < 0 || st >= forced.length) continue;
                    int f = forced[st];
                    if (f >= 0) return f;
                }
            }
        }
        return -1;
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

    private static boolean cellHits(double[] s, double[] b, long x, long y, long z) {
        if (s == null) return false;
        for (int k = 0; k < s.length; k += 6) {
            if (x + s[k + 3] > b[0]
                    && x + s[k] < b[3]
                    && y + s[k + 4] > b[1]
                    && y + s[k + 1] < b[4]
                    && z + s[k + 5] > b[2]
                    && z + s[k + 2] < b[5]) {
                return true;
            }
        }
        return false;
    }

    /// Returns true when the box `b` meets a collision shape of a
    /// block as the mob meets it. `b` holds the min and max x, y, z.
    private boolean collides(double[] b) {
        long x1 = (long) Math.floor(b[3] + EPS);
        long y1 = (long) Math.floor(b[4] + EPS);
        long z1 = (long) Math.floor(b[5] + EPS);
        for (long x = (long) Math.floor(b[0] - EPS); x <= x1; x++) {
            for (long y = (long) Math.floor(b[1] - EPS); y <= y1; y++) {
                for (long z = (long) Math.floor(b[2] - EPS); z <= z1; z++) {
                    int st = Chunk.blockAt(chunks, (int) x, (int) y, (int) z);
                    double[] s = Collision.shape(
                            shapes,
                            kinds,
                            st,
                            (int) x,
                            (int) y,
                            (int) z,
                            py,
                            ctx
                    );
                    if (cellHits(s, b, x, y, z)) return true;
                }
            }
        }
        return false;
    }

    /// Returns true when the mob box slides from where it stands to
    /// the node `n` without meeting a collision shape.
    private boolean canReach(PathNode n) {
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
            if (collides(b)) return false;
        }
        return true;
    }

    /// Returns the smallest straight distance from `n` to the
    /// `targets`, and lets each target keep `n` when `n` is the
    /// closest node to it so far.
    private static double nearestOffered(PathTarget[] targets, PathNode n) {
        double best = Float.MAX_VALUE;
        for (PathTarget t : targets) {
            double h = n.distTo(t.x, t.y, t.z);
            t.offer(h, n);
            best = Math.min(h, best);
        }
        return best;
    }

    /// Starts a search from the node `from` into `heap`.
    private static void start(PathHeap heap, PathTarget[] targets, PathNode from) {
        from.g = 0.0;
        from.h = PathNode.fl(nearestOffered(targets, from));
        from.f = from.h;
        heap.insert(from);
    }

    /// Lets the search step from `cur` onto its neighbour `n`. The
    /// step counts when the walked length stays under `maxlen` and
    /// it is the cheapest way to `n` so far.
    private static void relax(
            PathHeap heap,
            PathTarget[] targets,
            double maxlen,
            PathNode cur,
            PathNode n
    ) {
        double d = cur.distTo(n.x, n.y, n.z);
        double w = PathNode.fl(cur.walked + d);
        double g = PathNode.fl(PathNode.fl(cur.g + d) + n.malus());
        n.walked = w;
        boolean open = n.heapIdx >= 0;
        if (w < maxlen && (!open || g < n.g)) {
            n.came = cur;
            n.g = g;
            n.h = PathNode.fl(nearestOffered(targets, n) * FUDGING);
            if (open) {
                heap.changeCost(n, n.g + n.h);
            } else {
                n.f = PathNode.fl(n.g + n.h);
                heap.insert(n);
            }
        }
    }
}
