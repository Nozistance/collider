package collider.world.blocks;

import clojure.lang.Indexed;
import clojure.lang.RT;
import collider.world.Chunk;
import collider.world.ChunkIndex;
import java.util.Arrays;
import java.util.Map;

/// The spread of one liquid on its fluid tick. It finds the new liquid
/// of a cell and the lowest targets of the slope search, and it reads
/// the chunks under an overlay of states that the chunks lack yet.
/// Directions 0 to 3 go east, west, south and north, 4 up and 5 down.
/// A level is 0 for a source and 8 for a falling liquid. A flowing
/// liquid of amount n has the level 8 minus n.
public final class Flow {

    /// The fluid code of a state with no fluid.
    public static final int NONE = 0;

    /// The fluid code of water.
    public static final int WATER = 1;

    /// The fluid code of lava.
    public static final int LAVA = 2;

    /// The wall kind of a full cube.
    public static final int FULL = 0;

    /// The wall kind of a state with no collision.
    public static final int OPEN = 1;

    /// The wall kind of a state with some collision.
    public static final int PARTIAL = 2;

    /// The tables of one liquid, each indexed by block state.
    ///
    /// @param cls The fluid code of the liquid.
    /// @param fluid The fluid code of each state. A waterlogged state
    ///        holds water.
    /// @param level The level of the fluid of each state. A waterlogged
    ///        state holds a source.
    /// @param kinds The wall kind of each state.
    /// @param boxes The collision boxes of each state, six numbers per
    ///        box in sixteenths of a block.
    /// @param enterable True when the flow may enter the state.
    /// @param holeFloor True when a flow above the state sees a hole.
    /// @param holdsAny True when the state may hold some fluid.
    /// @param holdsSource True when the state may hold the source.
    /// @param holdsFlowing True when the state may hold the flow.
    /// @param ground True when a state below makes a new source.
    /// @param container True when the state holds a fluid in itself.
    /// @param drops True when a flow into the state drops it.
    /// @param base The source state of the liquid.
    /// @param voidAir The state outside the world height.
    public record Tables(
            int cls,
            byte[] fluid,
            byte[] level,
            byte[] kinds,
            double[][] boxes,
            boolean[] enterable,
            boolean[] holeFloor,
            boolean[] holdsAny,
            boolean[] holdsSource,
            boolean[] holdsFlowing,
            boolean[] ground,
            boolean[] container,
            boolean[] drops,
            int base,
            int voidAir) {}

    private static final int UP = 4;

    private static final int DOWN = 5;

    private static final int[] DX = {1, -1, 0, 0, 0, 0};

    private static final int[] DZ = {0, 0, 1, -1, 0, 0};

    private static final double HALF_FULL = 0.44444445;

    private static final int RAW = 0;

    private static final int BELOW = 1;

    private static final int HOLE = 2;

    private static final int PASS = 3;

    private static final int SLOTS = PASS + 4;

    private static final long EPOCH_MASK = 0xFFFFFFFF00000000L;

    private static final int FAR = 1000;

    private static final int TARGET = 2;

    private static final int CHANGE = 5;

    /// The per-thread arrays of the slope search and the face test. An
    /// entry holds its value in the low half and the epoch of the search
    /// that wrote it in the high half. Entries of older searches count
    /// as unset.
    private static final class Scratch {
        long[] cells = new long[SLOTS * 11 * 11];

        long[] seen = new long[11 * 11];

        int[] queue = new int[11 * 11];

        int[] rows = new int[16];

        long epoch;
    }

    private static final ThreadLocal<Scratch> SCRATCH =
            ThreadLocal.withInitial(Scratch::new);

    private final Tables t;

    private final ChunkIndex chunks;

    private int[] overX;

    private int[] overY;

    private int[] overZ;

    private int[] overState;

    private int overCount;

    private int[] made;

    private int madeCount;

    private int px;

    private int py;

    private int pz;

    private int radius;

    private int width;

    private int slope;

    private long[] cells;

    private long[] seen;

    private int[] queue;

    private int[] rows;

    private long stamp;

    private final Chunk[] near = new Chunk[4];

    private final int[] nearX = new int[4];

    private final int[] nearZ = new int[4];

    private Flow(Tables t, ChunkIndex chunks, Object over) {
        this.t = t;
        this.chunks = chunks;
        int n = over == null ? 0 : RT.count(over);
        overX = new int[n];
        overY = new int[n];
        overZ = new int[n];
        overState = new int[n];
        if (over != null) {
            int i = 0;
            for (Object o : (Iterable<?>) over) {
                Map.Entry<?, ?> e = (Map.Entry<?, ?>) o;
                Indexed k = (Indexed) e.getKey();
                overX[i] = RT.intCast(k.nth(0));
                overY[i] = RT.intCast(k.nth(1));
                overZ[i] = RT.intCast(k.nth(2));
                overState[i] = RT.intCast(e.getValue());
                i++;
            }
        }
        overCount = n;
    }

    /// Returns how many targets or changes a packed result holds.
    public static int count(int[] packed) {
        return packed[0];
    }

    /// Returns the direction of target k of a packed target list.
    public static int targetDirection(int[] found, int k) {
        return found[1 + TARGET * k];
    }

    /// Returns the level that target k of a packed target list takes.
    public static int targetLevel(int[] found, int k) {
        return found[2 + TARGET * k];
    }

    /// Returns the x of change k of a packed change list.
    public static int changeX(int[] changes, int k) {
        return changes[1 + CHANGE * k];
    }

    /// Returns the y of change k of a packed change list.
    public static int changeY(int[] changes, int k) {
        return changes[2 + CHANGE * k];
    }

    /// Returns the z of change k of a packed change list.
    public static int changeZ(int[] changes, int k) {
        return changes[3 + CHANGE * k];
    }

    /// Returns the new state of change k of a packed change list.
    public static int changeState(int[] changes, int k) {
        return changes[4 + CHANGE * k];
    }

    /// Returns the state that change k of a packed change list drops,
    /// or -1 when it drops none.
    public static int changeDrop(int[] changes, int k) {
        return changes[5 + CHANGE * k];
    }

    private void put(int x, int y, int z, int st) {
        for (int i = 0; i < overCount; i++) {
            if (overX[i] == x && overY[i] == y && overZ[i] == z) {
                overState[i] = st;
                return;
            }
        }
        if (overCount == overX.length) {
            int n = Math.max(8, 2 * overCount);
            overX = Arrays.copyOf(overX, n);
            overY = Arrays.copyOf(overY, n);
            overZ = Arrays.copyOf(overZ, n);
            overState = Arrays.copyOf(overState, n);
        }
        overX[overCount] = x;
        overY[overCount] = y;
        overZ[overCount] = z;
        overState[overCount] = st;
        overCount++;
    }

    private int raw(int x, int y, int z) {
        for (int i = 0; i < overCount; i++) {
            if (overX[i] == x && overY[i] == y && overZ[i] == z) {
                return overState[i];
            }
        }
        if (y < Chunk.MIN_Y || y > Chunk.MAX_Y) return t.voidAir();
        int cx = x >> 4;
        int cz = z >> 4;
        int k = (cx & 1) | (cz & 1) << 1;
        Chunk c = near[k];
        if (c == null || nearX[k] != cx || nearZ[k] != cz) {
            c = Chunk.at(chunks, cx, cz);
            near[k] = c;
            nearX[k] = cx;
            nearZ[k] = cz;
        }
        return c.block(x, y, z);
    }

    private static int axisOf(int d) {
        return d < 2 ? 0 : d < 4 ? 2 : 1;
    }

    private static void markFace(int[] rows, double[] boxes, int b, int u, int v) {
        int u0 = Math.max(0, (int) Math.ceil(boxes[b + u]));
        int u1 = Math.min(16, (int) Math.floor(boxes[b + u + 3]));
        int v0 = Math.max(0, (int) Math.ceil(boxes[b + v]));
        int v1 = Math.min(16, (int) Math.floor(boxes[b + v + 3]));
        if (u0 >= u1 || v0 >= v1) return;
        int m = ((1 << (v1 - v0)) - 1) << v0;
        for (int a = u0; a < u1; a++) {
            rows[a] |= m;
        }
    }

    private boolean faceCovered(double[] first, double[] second, int axis) {
        if (rows == null) rows = SCRATCH.get().rows;
        int u = axis == 0 ? 1 : 0;
        int v = axis == 2 ? 1 : 2;
        Arrays.fill(rows, 0);
        for (int b = 0; b < first.length; b += 6) {
            if (first[b + axis + 3] == 16.0) markFace(rows, first, b, u, v);
        }
        for (int b = 0; b < second.length; b += 6) {
            if (second[b + axis] == 0.0) markFace(rows, second, b, u, v);
        }
        for (int row : rows) {
            if (row != 0xFFFF) return false;
        }
        return true;
    }

    private boolean facesOpen(int src, int tgt, int d) {
        double[] s = t.boxes()[src];
        double[] g = t.boxes()[tgt];
        int axis = axisOf(d);
        boolean ahead = d == 0 || d == 2 || d == UP;
        return ahead ? !faceCovered(s, g, axis) : !faceCovered(g, s, axis);
    }

    private boolean facesPass(int src, int tgt, int d) {
        byte[] k = t.kinds();
        int ks = k[src];
        int kt = k[tgt];
        if (ks == FULL || kt == FULL) return false;
        if (ks == OPEN && kt == OPEN) return true;
        return facesOpen(src, tgt, d);
    }

    private boolean same(int st) {
        return t.fluid()[st] == t.cls();
    }

    private boolean source(int st) {
        return same(st) && t.level()[st] == 0;
    }

    private int amount(int st) {
        int l = t.level()[st];
        return (l == 0 || l >= 8) ? 8 : 8 - l;
    }

    private int newLiquid(int x, int y, int z, int dropoff, boolean infinite) {
        int raw = raw(x, y, z);
        int highest = 0;
        int sources = 0;
        for (int d = 0; d < 4; d++) {
            int n = raw(x + DX[d], y, z + DZ[d]);
            if (same(n) && facesPass(raw, n, d)) {
                highest = Math.max(highest, amount(n));
                if (t.level()[n] == 0) sources++;
            }
        }
        int below = raw(x, y - 1, z);
        int above = raw(x, y + 1, z);
        if (infinite && sources >= 2 && t.ground()[below]) return 0;
        if (same(above) && facesPass(raw, above, UP)) return 8;
        int n = highest - dropoff;
        return n > 0 ? 8 - n : -1;
    }

    /// Returns the level of the liquid that the cell at `x` `y` `z`
    /// takes from its neighbours, or -1 when it takes none.
    public static int newLiquid(
            Tables t,
            ChunkIndex chunks,
            Object over,
            int x,
            int y,
            int z,
            int dropoff,
            boolean infinite
    ) {
        return new Flow(t, chunks, over).newLiquid(x, y, z, dropoff, infinite);
    }

    private boolean replaceableDown(int st) {
        int f = t.fluid()[st];
        if (f == NONE) return true;
        if (f == WATER) return t.cls() != WATER;
        return t.cls() == WATER && amount(st) / 9.0 >= HALF_FULL;
    }

    /// Returns the level of the liquid that the liquid at `x` `y` `z`
    /// spreads down to, or -1 when it does not spread down.
    public static int downLevel(
            Tables t,
            ChunkIndex chunks,
            Object over,
            int x,
            int y,
            int z,
            int dropoff,
            boolean infinite
    ) {
        return new Flow(t, chunks, over).down(x, y, z, dropoff, infinite);
    }

    /// Returns true when the liquid at `x` `y` `z` has a hole below.
    public static boolean hole(
            Tables t,
            ChunkIndex chunks,
            Object over,
            int x,
            int y,
            int z
    ) {
        return new Flow(t, chunks, over).holeAt(x, y, z);
    }

    /// Returns true when lava is at one of the six cells around `x`
    /// `y` `z`.
    public static boolean lavaNear(
            Tables t,
            ChunkIndex chunks,
            Object over,
            int x,
            int y,
            int z
    ) {
        return new Flow(t, chunks, over).lavaAround(x, y, z);
    }

    private int index(int x, int z) {
        return (x - px + radius) * width + (z - pz + radius);
    }

    private int cellRaw(int i, int dy) {
        int k = i * SLOTS + (dy == 0 ? RAW : BELOW);
        long v = cells[k];
        if ((v & EPOCH_MASK) == stamp) return (int) v;
        int s = raw(px - radius + i / width, py + dy, pz - radius + i % width);
        cells[k] = stamp | s;
        return s;
    }

    private boolean holeAtIndex(int i) {
        int k = i * SLOTS + HOLE;
        long v = cells[k];
        if ((v & EPOCH_MASK) == stamp) return (int) v == 1;
        int braw = cellRaw(i, -1);
        boolean h = facesPass(cellRaw(i, 0), braw, DOWN) && t.holeFloor()[braw];
        cells[k] = stamp | (h ? 1 : 0);
        return h;
    }

    private int step(int i, int d) {
        return i + DX[d] * width + DZ[d];
    }

    private boolean passable(int i, int d) {
        int k = i * SLOTS + PASS + d;
        long v = cells[k];
        if ((v & EPOCH_MASK) == stamp) return (int) v == 1;
        int traw = cellRaw(i, 0);
        boolean ok = t.enterable()[traw]
                && facesPass(cellRaw(step(i, d ^ 1), 0), traw, d);
        cells[k] = stamp | (ok ? 1 : 0);
        return ok;
    }

    private int slopeDistance(int c, int back, long mark) {
        seen[c] = mark;
        queue[0] = c;
        int head = 0;
        int tail = 1;
        for (int pass = 1; pass <= slope; pass++) {
            for (int end = tail; head < end; head++) {
                int i = queue[head];
                for (int d = 0; d < 4; d++) {
                    int n = step(i, d);
                    if (i == c && d == back || seen[n] == mark || !passable(n, d)) {
                        continue;
                    }
                    if (holeAtIndex(n)) return pass;
                    seen[n] = mark;
                    queue[tail++] = n;
                }
            }
        }
        return FAR;
    }

    private boolean replaceable(int st) {
        int f = t.fluid()[st];
        if (f == NONE) return true;
        if (f == t.cls()) return false;
        return t.cls() == WATER && amount(st) / 9.0 >= HALF_FULL;
    }

    private boolean candidate(int raw, int d, int[] out, int dropoff, boolean infinite) {
        int tx = px + DX[d];
        int tz = pz + DZ[d];
        int traw = cellRaw(index(tx, tz), 0);
        if (source(traw) || !t.holdsAny()[traw] || !facesPass(raw, traw, d)) {
            return false;
        }
        int v = newLiquid(tx, py, tz, dropoff, infinite);
        if (v < 0) return false;
        boolean[] holds = v == 0 ? t.holdsSource() : t.holdsFlowing();
        if (!holds[traw]) return false;
        out[d] = v;
        return true;
    }

    private int side(int d) {
        return index(px + DX[d], pz + DZ[d]);
    }

    private void begin(int x, int y, int z, int slope) {
        px = x;
        py = y;
        pz = z;
        this.slope = slope;
        radius = slope + 1;
        width = 2 * radius + 1;
        Scratch sc = SCRATCH.get();
        if (++sc.epoch == Integer.MAX_VALUE) {
            Arrays.fill(sc.cells, 0L);
            Arrays.fill(sc.seen, 0L);
            sc.epoch = 1;
        }
        stamp = sc.epoch << 32;
        int n = width * width;
        boolean fit = n <= sc.seen.length;
        cells = fit ? sc.cells : new long[SLOTS * n];
        seen = fit ? sc.seen : new long[n];
        queue = fit ? sc.queue : new int[n];
    }

    private void distances(int[] levels, int[] dist) {
        boolean holes = false;
        for (int d = 0; d < 4; d++) {
            dist[d] = levels[d] >= 0 && holeAtIndex(side(d)) ? 0 : FAR;
            holes |= dist[d] == 0;
        }
        for (int d = 0; d < 4 && !holes; d++) {
            if (levels[d] < 0) continue;
            dist[d] = slopeDistance(side(d), d ^ 1, stamp | (d + 1));
        }
    }

    private int[] lowestTargets(
            int x,
            int y,
            int z,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        begin(x, y, z, slope);
        int raw = raw(x, y, z);
        int[] levels = {-1, -1, -1, -1};
        boolean[] fits = new boolean[4];
        int open = 0;
        int fit = 0;
        for (int d = 0; d < 4; d++) {
            if (!candidate(raw, d, levels, dropoff, infinite)) continue;
            open++;
            fits[d] = replaceable(cellRaw(side(d), 0));
            if (fits[d]) fit++;
        }
        if (fit > 0 && open > 1) {
            int[] dist = new int[4];
            distances(levels, dist);
            int lowest = FAR;
            for (int d = 0; d < 4; d++) {
                if (levels[d] >= 0) lowest = Math.min(lowest, dist[d]);
            }
            for (int d = 0; d < 4; d++) {
                fits[d] = fits[d] && dist[d] == lowest;
            }
        }
        int[] out = new int[1 + TARGET * 4];
        for (int d = 3; d >= 0; d--) {
            if (fits[d]) {
                out[1 + TARGET * out[0]] = d;
                out[2 + TARGET * out[0]] = levels[d];
                out[0]++;
            }
        }
        return out;
    }

    /// Returns the targets that the liquid at `x` `y` `z` spreads to on
    /// its sides, north first, then south, west and east. Read the
    /// result with `count`, `targetDirection` and `targetLevel`.
    public static int[] lowestTargets(
            Tables t,
            ChunkIndex chunks,
            Object over,
            int x,
            int y,
            int z,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        return new Flow(t, chunks, over)
                .lowestTargets(x, y, z, dropoff, slope, infinite);
    }

    private void add(int x, int y, int z, int st, int dropped) {
        if (CHANGE * madeCount + CHANGE + 1 > made.length) {
            made = Arrays.copyOf(made, 2 * made.length);
        }
        int k = 1 + CHANGE * madeCount++;
        made[k] = x;
        made[k + 1] = y;
        made[k + 2] = z;
        made[k + 3] = st;
        made[k + 4] = dropped;
    }

    private boolean spreadTo(int x, int y, int z, int v) {
        if (y < Chunk.MIN_Y || y > Chunk.MAX_Y) return true;
        int traw = raw(x, y, z);
        if (t.container()[traw] || lavaAround(x, y, z)) return false;
        int st = t.base() + v;
        add(x, y, z, st, t.drops()[traw] ? traw : -1);
        put(x, y, z, st);
        return true;
    }

    private boolean lavaAround(int x, int y, int z) {
        for (int d = 0; d < 6; d++) {
            int dy = d == UP ? 1 : d == DOWN ? -1 : 0;
            if (t.fluid()[raw(x + DX[d], y + dy, z + DZ[d])] == LAVA) return true;
        }
        return false;
    }

    private int down(int x, int y, int z, int dropoff, boolean infinite) {
        int raw = raw(x, y, z);
        int braw = raw(x, y - 1, z);
        if (source(braw) || !t.holdsAny()[braw] || !facesPass(raw, braw, DOWN)) {
            return -1;
        }
        int v = newLiquid(x, y - 1, z, dropoff, infinite);
        if (v < 0 || !replaceableDown(braw)) return -1;
        boolean[] holds = v == 0 ? t.holdsSource() : t.holdsFlowing();
        return holds[braw] ? v : -1;
    }

    private boolean holeAt(int x, int y, int z) {
        int braw = raw(x, y - 1, z);
        return facesPass(raw(x, y, z), braw, DOWN) && t.holeFloor()[braw];
    }

    private int sourcesAround(int x, int y, int z) {
        int n = 0;
        for (int d = 0; d < 4; d++) {
            if (source(raw(x + DX[d], y, z + DZ[d]))) n++;
        }
        return n;
    }

    private boolean sides(
            int x,
            int y,
            int z,
            int l,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        int n = l == 8 ? 7 : (l == 0 ? 8 : 8 - l) - dropoff;
        if (n <= 0) return true;
        int[] found = lowestTargets(x, y, z, dropoff, slope, infinite);
        for (int k = 0; k < count(found); k++) {
            int d = targetDirection(found, k);
            if (!spreadTo(x + DX[d], y, z + DZ[d], targetLevel(found, k))) {
                return false;
            }
        }
        return true;
    }

    private boolean spread(
            int x,
            int y,
            int z,
            int l,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        int v = down(x, y, z, dropoff, infinite);
        if (v >= 0) {
            return spreadTo(x, y - 1, z, v)
                    && (sourcesAround(x, y, z) < 3
                            || sides(x, y, z, l, dropoff, slope, infinite));
        }
        if (l == 0 || !holeAt(x, y, z)) {
            return sides(x, y, z, l, dropoff, slope, infinite);
        }
        return true;
    }

    private int[] waterChanges(
            int x,
            int y,
            int z,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        int raw = raw(x, y, z);
        if (!same(raw)) return null;
        int l = t.level()[raw];
        int v = l == 0 ? 0 : newLiquid(x, y, z, dropoff, infinite);
        made = new int[16];
        if (v < 0) {
            add(x, y, z, 0, -1);
        } else {
            if (v != l) {
                add(x, y, z, t.base() + v, -1);
                put(x, y, z, t.base() + v);
            }
            if (!spread(x, y, z, v, dropoff, slope, infinite)) {
                return null;
            }
        }
        made[0] = madeCount;
        return made;
    }

    /// Returns the changes of the water at `x` `y` `z` on its fluid
    /// tick, or null when one of them needs the full path, as for a
    /// container or for lava next to the flow. Read the result with
    /// `count` and the `change` accessors.
    public static int[] changes(
            Tables t,
            ChunkIndex chunks,
            int x,
            int y,
            int z,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        return new Flow(t, chunks, null)
                .waterChanges(x, y, z, dropoff, slope, infinite);
    }
}
