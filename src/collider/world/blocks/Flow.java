package collider.world.blocks;

import clojure.lang.IFn;
import clojure.lang.Indexed;
import clojure.lang.RT;
import collider.world.Chunk;
import collider.world.ChunkIndex;
import java.util.Arrays;
import java.util.Map;

/// The spread of one liquid on its fluid tick, as vanilla's
/// FlowingFluid: the new liquid of a cell and the lowest targets of
/// the slope search. It reads the chunks under an overlay of states
/// that the chunks lack yet. Directions 0 to 3 go east, west, south
/// and north, 4 up and 5 down. A level is 0 for a source, 8 for a
/// falling liquid and 8 - n for a flowing liquid of amount n.
public final class Flow {

    /// The tables of one liquid, each indexed by block state. `fluid`
    /// is 0, 1 for water or 2 for lava and `level` the level of the
    /// fluid that a state holds, a waterlogged state as a water
    /// source. `kinds` is 0 for a full cube, 1 for no collision and 2
    /// for other walls, whose faces `facesOpen` tells of.
    ///
    /// @param cls The fluid of the liquid.
    /// @param fluid The fluid of each state.
    /// @param level The level of each state.
    /// @param kinds The wall of each state.
    /// @param enterable True when the flow may enter the state.
    /// @param holeFloor True when a flow above the state sees a hole.
    /// @param holdsAny True when the state may hold some fluid.
    /// @param holdsSource True when the state may hold the source.
    /// @param holdsFlowing True when the state may hold the flow.
    /// @param ground True when a state below makes a new source.
    /// @param voidAir The state outside the world height.
    /// @param facesOpen Takes a source state, a target state and a
    ///        direction and tells whether the flow passes.
    public record Tables(int cls, byte[] fluid, byte[] level,
                         byte[] kinds, boolean[] enterable,
                         boolean[] holeFloor, boolean[] holdsAny,
                         boolean[] holdsSource,
                         boolean[] holdsFlowing,
                         boolean[] ground, int voidAir,
                         IFn facesOpen) {}

    private static final int MIN_Y = -64;

    private static final int MAX_Y = 319;

    private static final int UP = 4;

    private static final int DOWN = 5;

    private static final int[] DX = {1, -1, 0, 0, 0, 0};

    private static final int[] DZ = {0, 0, 1, -1, 0, 0};

    private static final int SLOTS = 7;

    private static final ThreadLocal<long[]> CELLS =
            ThreadLocal.withInitial(() -> new long[SLOTS * 11 * 11]);

    private final Tables t;

    private final ChunkIndex chunks;

    private final int[] ox, oy, oz, ost;

    private final int on;

    private int px, py, pz, r, w, slope;

    private long[] cells;

    private Chunk last;

    private int lastX, lastZ;

    private Flow(Tables t, ChunkIndex chunks, Object over) {
        this.t = t;
        this.chunks = chunks;
        int n = over == null ? 0 : RT.count(over);
        ox = new int[n]; oy = new int[n]; oz = new int[n];
        ost = new int[n];
        int i = 0;
        if (n > 0) {
            for (Object o : (Iterable<?>) over) {
                Map.Entry<?, ?> e = (Map.Entry<?, ?>) o;
                Indexed k = (Indexed) e.getKey();
                ox[i] = RT.intCast(k.nth(0));
                oy[i] = RT.intCast(k.nth(1));
                oz[i] = RT.intCast(k.nth(2));
                ost[i] = RT.intCast(e.getValue());
                i++;
            }
        }
        on = n;
    }

    private int raw(int x, int y, int z) {
        for (int i = 0; i < on; i++) {
            if (ox[i] == x && oy[i] == y && oz[i] == z) return ost[i];
        }
        if (y < MIN_Y || y > MAX_Y) return t.voidAir();
        int cx = x >> 4, cz = z >> 4;
        if (last == null || cx != lastX || cz != lastZ) {
            last = Chunk.at(chunks, cx, cz);
            lastX = cx;
            lastZ = cz;
        }
        return last.block(x, y, z);
    }

    private boolean pass(int src, int tgt, int d) {
        byte[] k = t.kinds();
        int ks = k[src], kt = k[tgt];
        if (ks == 0 || kt == 0) return false;
        if (ks == 1 && kt == 1) return true;
        return RT.booleanCast(t.facesOpen().invoke(src, tgt, d));
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

    private int newLiquid(int x, int y, int z, int dropoff,
            boolean infinite) {
        int raw = raw(x, y, z);
        int highest = 0, sources = 0;
        for (int d = 0; d < 4; d++) {
            int n = raw(x + DX[d], y, z + DZ[d]);
            if (same(n) && pass(raw, n, d)) {
                highest = Math.max(highest, amount(n));
                if (t.level()[n] == 0) sources++;
            }
        }
        int below = raw(x, y - 1, z), above = raw(x, y + 1, z);
        if (infinite && sources >= 2 && t.ground()[below]) return 0;
        if (same(above) && pass(raw, above, UP)) return 8;
        int n = highest - dropoff;
        return n > 0 ? 8 - n : -1;
    }

    /// Returns the level of the liquid that the cell at `x` `y` `z`
    /// takes from its neighbours, or -1 when it takes none.
    public static int newLiquid(Tables t, ChunkIndex chunks,
            Object over, int x, int y, int z, int dropoff,
            boolean infinite) {
        return new Flow(t, chunks, over)
                .newLiquid(x, y, z, dropoff, infinite);
    }

    private boolean replaceableDown(int st) {
        int f = t.fluid()[st];
        if (f == 0) return true;
        if (f == 1) return t.cls() != 1;
        return t.cls() == 1 && amount(st) / 9.0 >= 0.44444445;
    }

    /// Returns the level of the liquid that the liquid at `x` `y` `z`
    /// spreads down to, or -1 when it spreads not down.
    public static int downLevel(Tables t, ChunkIndex chunks,
            Object over, int x, int y, int z, int dropoff,
            boolean infinite) {
        Flow f = new Flow(t, chunks, over);
        int raw = f.raw(x, y, z), braw = f.raw(x, y - 1, z);
        if (f.source(braw) || !t.holdsAny()[braw]
                || !f.pass(raw, braw, DOWN)) return -1;
        int v = f.newLiquid(x, y - 1, z, dropoff, infinite);
        if (v < 0 || !f.replaceableDown(braw)) return -1;
        boolean[] holds = v == 0 ? t.holdsSource() : t.holdsFlowing();
        return holds[braw] ? v : -1;
    }

    /// Returns true when the liquid at `x` `y` `z` has a hole below.
    public static boolean hole(Tables t, ChunkIndex chunks,
            Object over, int x, int y, int z) {
        Flow f = new Flow(t, chunks, over);
        int braw = f.raw(x, y - 1, z);
        return f.pass(f.raw(x, y, z), braw, DOWN)
                && t.holeFloor()[braw];
    }

    /// Returns true when lava is at one of the six cells around `x`
    /// `y` `z`.
    public static boolean lavaNear(Tables t, ChunkIndex chunks,
            Object over, int x, int y, int z) {
        Flow f = new Flow(t, chunks, over);
        for (int d = 0; d < 6; d++) {
            int dy = d == UP ? 1 : d == DOWN ? -1 : 0;
            if (t.fluid()[f.raw(x + DX[d], y + dy, z + DZ[d])] == 2)
                return true;
        }
        return false;
    }

    private int cell(int x, int z) {
        return (x - px + r) * w + (z - pz + r);
    }

    private int cellRaw(int i, int dy) {
        int k = i * SLOTS + (dy == 0 ? 0 : 1);
        long v = cells[k];
        if (v >= 0) return (int) v;
        int s = raw(px - r + i / w, py + dy, pz - r + i % w);
        cells[k] = s;
        return s;
    }

    private boolean hole(int i) {
        int k = i * SLOTS + 2;
        long v = cells[k];
        if (v >= 0) return v == 1;
        int braw = cellRaw(i, -1);
        boolean h = pass(cellRaw(i, 0), braw, DOWN)
                && t.holeFloor()[braw];
        cells[k] = h ? 1 : 0;
        return h;
    }

    private int step(int i, int d) {
        return i + DX[d] * w + DZ[d];
    }

    private boolean passable(int i, int d) {
        int k = i * SLOTS + 3 + d;
        long v = cells[k];
        if (v >= 0) return v == 1;
        int traw = cellRaw(i, 0);
        boolean ok = t.enterable()[traw]
                && pass(cellRaw(step(i, d ^ 1), 0), traw, d);
        cells[k] = ok ? 1 : 0;
        return ok;
    }

    private int slopeDistance(int i, int pass, int from) {
        int lowest = 1000;
        for (int d = 0; d < 4; d++) {
            int n = step(i, d);
            if (d == from || !passable(n, d)) continue;
            if (hole(n)) return pass;
            if (pass < slope) {
                lowest = Math.min(lowest,
                        slopeDistance(n, pass + 1, d ^ 1));
            }
        }
        return lowest;
    }

    private boolean replaceable(int st) {
        int f = t.fluid()[st];
        if (f == 0) return true;
        if (f == t.cls()) return false;
        return t.cls() == 1 && amount(st) / 9.0 >= 0.44444445;
    }

    private int distance(int raw, int d, int[] out, int dropoff,
            boolean infinite) {
        int tx = px + DX[d], tz = pz + DZ[d];
        int i = cell(tx, tz);
        int traw = cellRaw(i, 0);
        if (source(traw) || !t.holdsAny()[traw]
                || !pass(raw, traw, d)) return -1;
        int v = newLiquid(tx, py, tz, dropoff, infinite);
        if (v < 0) return -1;
        boolean[] holds = v == 0 ? t.holdsSource() : t.holdsFlowing();
        if (!holds[traw]) return -1;
        out[d] = v;
        return hole(i) ? 0 : slopeDistance(i, 1, d ^ 1);
    }

    private int[] lowestTargets(int x, int y, int z, int dropoff,
            int slope, boolean infinite) {
        px = x; py = y; pz = z; this.slope = slope;
        r = slope + 1; w = 2 * r + 1;
        int n = SLOTS * w * w;
        long[] c = CELLS.get();
        cells = n <= c.length ? c : new long[n];
        Arrays.fill(cells, 0, n, -1L);
        int raw = raw(x, y, z);
        int[] levels = new int[4];
        boolean[] kept = new boolean[4];
        int lowest = 1000;
        for (int d = 0; d < 4; d++) {
            int dist = distance(raw, d, levels, dropoff, infinite);
            if (dist < 0 || dist > lowest) continue;
            if (dist < lowest) Arrays.fill(kept, false);
            lowest = dist;
            int i = cell(x + DX[d], z + DZ[d]);
            kept[d] = replaceable(cellRaw(i, 0));
        }
        int[] out = new int[9];
        for (int d = 3; d >= 0; d--) {
            if (kept[d]) {
                out[1 + 2 * out[0]] = d;
                out[2 + 2 * out[0]] = levels[d];
                out[0]++;
            }
        }
        return out;
    }

    /// Returns the targets that the liquid at `x` `y` `z` spreads to
    /// on its sides: their count, then a direction and a level each,
    /// north first, then south, west and east.
    public static int[] lowestTargets(Tables t, ChunkIndex chunks,
            Object over, int x, int y, int z, int dropoff, int slope,
            boolean infinite) {
        return new Flow(t, chunks, over)
                .lowestTargets(x, y, z, dropoff, slope, infinite);
    }
}
