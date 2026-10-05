package collider.world.blocks;

import collider.world.Chunk;
import collider.world.ChunkIndex;
import java.util.Arrays;

/// The spread of one liquid on its fluid tick. It finds the changes
/// of the tick with their effects, and the lava next to each change
/// that mixes with water as the change reaches it. It reads the chunks
/// under an overlay of the states that the tick changed.
/// Directions 0 to 3 go east, west, south and north, 4 up and 5 down.
/// A level is 0 for a source and 8 for a falling liquid. A flowing
/// liquid of amount n has the level 8 minus n.
public final class Flow {

    public static final int NONE = 0;

    public static final int WATER = 1;

    public static final int LAVA = 2;

    /// The wall kind of a full cube.
    public static final int FULL = 0;

    /// The wall kind of a state with no collision.
    public static final int OPEN = 1;

    /// The wall kind of a state with some collision.
    public static final int PARTIAL = 2;

    /// The effect of a lava flow that breaks a block or mixes.
    public static final int FIZZ = 1;

    /// The effect of a lava flow that breaks a block and mixes.
    public static final int FIZZ_TWICE = 2;

    /// The effect of a container that takes water.
    public static final int FILL = 3;

    /// The effect of a lit container that takes water and goes out.
    public static final int DOUSE = 4;

    /// The effect of a dried ghast that takes water.
    public static final int SOAK = 5;

    /// The blocks that lava turns into next to water.
    ///
    /// @param source The block of a lava source over water.
    /// @param flowing The block of a flowing lava over water.
    /// @param smother The block of water that lava falls into.
    /// @param basalt The block of lava over soul soil by blue ice.
    /// @param soulSoil The state below lava that makes basalt.
    /// @param blueIce The state beside lava that makes basalt.
    public record Mix(
            int source,
            int flowing,
            int smother,
            int basalt,
            int soulSoil,
            int blueIce) {}

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
    /// @param held The state that a container takes water as.
    /// @param fills The effect of a container that takes water.
    /// @param drops True when a flow into the state drops it.
    /// @param mix The blocks of lava next to water.
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
            long[] held,
            byte[] fills,
            boolean[] drops,
            Mix mix,
            int base,
            int voidAir) {}

    private static final int EAST = 0;

    private static final int WEST = 1;

    private static final int SOUTH = 2;

    private static final int NORTH = 3;

    private static final int UP = 4;

    private static final int DOWN = 5;

    private static final int[] DX = {1, -1, 0, 0, 0, 0};

    private static final int[] DY = {0, 0, 0, 0, 1, -1};

    private static final int[] DZ = {0, 0, 1, -1, 0, 0};

    private static final int[] MIXES = {UP, NORTH, SOUTH, WEST, EAST};

    private static final int[] UPDATES = {WEST, EAST, DOWN, UP, NORTH, SOUTH};

    private static final double HALF_FULL = 0.44444445;

    private static final int RAW = 0;

    private static final int BELOW = 1;

    private static final int HOLE = 2;

    private static final int PASS = 3;

    private static final int SLOTS = PASS + 4;

    private static final long EPOCH_MASK = 0xFFFFFFFF00000000L;

    private static final int FAR = 1000;

    private static final int TARGET = 2;

    private static final int CHANGE = 6;

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

    private int[] overX = new int[0];

    private int[] overY = new int[0];

    private int[] overZ = new int[0];

    private int[] overState = new int[0];

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

    private final int[] mixedAround = new int[UPDATES.length];

    private Flow(Tables t, ChunkIndex chunks) {
        this.t = t;
        this.chunks = chunks;
    }

    public static int count(int[] packed) {
        return packed[0];
    }

    private static int targetDirection(int[] found, int k) {
        return found[1 + TARGET * k];
    }

    private static int targetLevel(int[] found, int k) {
        return found[2 + TARGET * k];
    }

    public static int changeX(int[] changes, int k) {
        return changes[1 + CHANGE * k];
    }

    public static int changeY(int[] changes, int k) {
        return changes[2 + CHANGE * k];
    }

    public static int changeZ(int[] changes, int k) {
        return changes[3 + CHANGE * k];
    }

    public static int changeState(int[] changes, int k) {
        return changes[4 + CHANGE * k];
    }

    /// Returns the state that change k of a packed change list drops,
    /// or -1 when it drops none.
    public static int changeDrop(int[] changes, int k) {
        return changes[5 + CHANGE * k];
    }

    /// Returns the effect of change k of a packed change list, or 0 when
    /// it has none.
    public static int changeEffect(int[] changes, int k) {
        return changes[6 + CHANGE * k];
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

    private boolean replaceableDown(int st) {
        int f = t.fluid()[st];
        if (f == NONE) return true;
        if (f == WATER) return t.cls() != WATER;
        return t.cls() == WATER && amount(st) / 9.0 >= HALF_FULL;
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

    private void add(int x, int y, int z, int st, int dropped, int effect) {
        if (CHANGE * madeCount + CHANGE + 1 > made.length) {
            made = Arrays.copyOf(made, 2 * made.length);
        }
        int k = 1 + CHANGE * madeCount++;
        made[k] = x;
        made[k + 1] = y;
        made[k + 2] = z;
        made[k + 3] = st;
        made[k + 4] = dropped;
        made[k + 5] = effect;
    }

    private int mixed(int x, int y, int z) {
        int st = raw(x, y, z);
        if (t.fluid()[st] != LAVA) return -1;
        Mix m = t.mix();
        boolean soul = raw(x, y - 1, z) == m.soulSoil();
        for (int d : MIXES) {
            int n = raw(x + DX[d], y + DY[d], z + DZ[d]);
            if (t.fluid()[n] == WATER) {
                return t.level()[st] == 0 ? m.source() : m.flowing();
            }
            if (soul && n == m.blueIce()) return m.basalt();
        }
        return -1;
    }

    private void placeAndMixAround(int x, int y, int z, int st) {
        put(x, y, z, st);
        for (int k = 0; k < UPDATES.length; k++) {
            int d = UPDATES[k];
            mixedAround[k] = mixed(x + DX[d], y + DY[d], z + DZ[d]);
        }
        for (int k = 0; k < UPDATES.length; k++) {
            int d = UPDATES[k];
            if (mixedAround[k] >= 0) {
                put(x + DX[d], y + DY[d], z + DZ[d], mixedAround[k]);
            }
        }
    }

    private void flowInto(int x, int y, int z, int v, int traw) {
        boolean lava = t.cls() == LAVA;
        int st = t.base() + v;
        int mix = -1;
        if (lava) {
            put(x, y, z, st);
            mix = mixed(x, y, z);
        }
        boolean gone = t.drops()[traw];
        int fizz = (lava && gone ? 1 : 0) + (mix >= 0 ? 1 : 0);
        int end = mix >= 0 ? mix : st;
        add(x, y, z, end, !lava && gone ? traw : -1, fizz);
        placeAndMixAround(x, y, z, end);
    }

    private void spreadTo(int x, int y, int z, int v, int d) {
        if (y < Chunk.MIN_Y || y > Chunk.MAX_Y) return;
        int traw = raw(x, y, z);
        if (t.cls() == LAVA && d == DOWN && t.fluid()[traw] == WATER) {
            add(x, y, z, t.mix().smother(), -1, FIZZ);
            put(x, y, z, t.mix().smother());
        } else if (t.container()[traw]) {
            int st = (int) t.held()[traw];
            add(x, y, z, st, -1, t.fills()[traw]);
            placeAndMixAround(x, y, z, st);
        } else {
            flowInto(x, y, z, v, traw);
        }
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

    private void sides(
            int x,
            int y,
            int z,
            int l,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        int n = l == 8 ? 7 : (l == 0 ? 8 : 8 - l) - dropoff;
        if (n <= 0) return;
        int[] found = lowestTargets(x, y, z, dropoff, slope, infinite);
        for (int k = 0; k < count(found); k++) {
            int d = targetDirection(found, k);
            spreadTo(x + DX[d], y, z + DZ[d], targetLevel(found, k), d);
        }
    }

    private void spread(
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
            spreadTo(x, y - 1, z, v, DOWN);
            if (sourcesAround(x, y, z) >= 3) {
                sides(x, y, z, l, dropoff, slope, infinite);
            }
        } else if (l == 0 || !holeAt(x, y, z)) {
            sides(x, y, z, l, dropoff, slope, infinite);
        }
    }

    private int[] changes(
            int x,
            int y,
            int z,
            int dropoff,
            int slope,
            boolean infinite
    ) {
        int raw = raw(x, y, z);
        int l = t.level()[raw];
        int v = l == 0 ? 0 : newLiquid(x, y, z, dropoff, infinite);
        made = new int[16];
        if (v < 0) {
            add(x, y, z, 0, -1, 0);
        } else {
            if (v != l) {
                add(x, y, z, t.base() + v, -1, 0);
                put(x, y, z, t.base() + v);
            }
            spread(x, y, z, v, dropoff, slope, infinite);
        }
        made[0] = madeCount;
        return made;
    }

    /// Returns the changes of the liquid at `x` `y` `z` on its fluid
    /// tick. Read the result with `count` and the `change` accessors.
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
        return new Flow(t, chunks).changes(x, y, z, dropoff, slope, infinite);
    }

    /// Returns the block that the lava at `x` `y` `z` turns into, or -1
    /// when it stays lava.
    public static int mixed(Tables t, ChunkIndex chunks, int x, int y, int z) {
        boolean inside = y >= Chunk.MIN_Y && y <= Chunk.MAX_Y;
        if (!inside || t.fluid()[Chunk.blockAt(chunks, x, y, z)] != LAVA) {
            return -1;
        }
        return new Flow(t, chunks).mixed(x, y, z);
    }
}
