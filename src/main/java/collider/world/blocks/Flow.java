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
    /// @param container True when the state holds a fluid in itself.
    /// @param drops True when a flow into the state drops it.
    /// @param base The source state of the liquid.
    /// @param voidAir The state outside the world height.
    /// @param facesOpen Takes a source state, a target state and a
    ///        direction and tells whether the flow passes.
    public record Tables(
            int cls,
            byte[] fluid,
            byte[] level,
            byte[] kinds,
            boolean[] enterable,
            boolean[] holeFloor,
            boolean[] holdsAny,
            boolean[] holdsSource,
            boolean[] holdsFlowing,
            boolean[] ground,
            boolean[] container,
            boolean[] drops,
            int base,
            int voidAir,
            IFn facesOpen) {}

    private static final int UP = 4;

    private static final int DOWN = 5;

    private static final int[] DX = {1, -1, 0, 0, 0, 0};

    private static final int[] DZ = {0, 0, 1, -1, 0, 0};

    private static final int SLOTS = 7;

    private static final long HI = 0xFFFFFFFF00000000L;

    /// The per-thread arrays of the slope search. An entry holds its
    /// value in the low half and the epoch of the search that wrote
    /// it in the high half; entries of older searches count as unset.
    private static final class SlopeSearch {
        long[] cells = new long[SLOTS * 11 * 11];
        long[] seen = new long[11 * 11];
        int[] queue = new int[11 * 11];
        long epoch;
    }

    private static final ThreadLocal<SlopeSearch> SEARCH = ThreadLocal.withInitial(SlopeSearch::new);

    private final Tables t;

    private final ChunkIndex chunks;

    private int[] ox, oy, oz, ost;

    private int on;

    private int[] made;

    private int mn;

    private int px, py, pz, r, w, slope;
    private long[] cells, seen;

    private int[] queue;

    private long stamp;

    private final Chunk[] near = new Chunk[4];

    private final int[] nearX = new int[4], nearZ = new int[4];

    private Flow(Tables t, ChunkIndex chunks, Object over) {
        this.t = t;
        this.chunks = chunks;
        int n = over == null ? 0 : RT.count(over);
        ox = new int[n];
        oy = new int[n];
        oz = new int[n];
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

    private void put(int x, int y, int z, int st) {
        for (int i = 0; i < on; i++) {
            if (ox[i] == x && oy[i] == y && oz[i] == z) {
                ost[i] = st;
                return;
            }
        }
        if (on == ox.length) {
            int n = Math.max(8, 2 * on);
            ox = Arrays.copyOf(ox, n);
            oy = Arrays.copyOf(oy, n);
            oz = Arrays.copyOf(oz, n);
            ost = Arrays.copyOf(ost, n);
        }
        ox[on] = x;
        oy[on] = y;
        oz[on] = z;
        ost[on] = st;
        on++;
    }

    private int raw(int x, int y, int z) {
        for (int i = 0; i < on; i++) {
            if (ox[i] == x && oy[i] == y && oz[i] == z) return ost[i];
        }
        if (y < Chunk.MIN_Y || y > Chunk.MAX_Y) return t.voidAir();
        int cx = x >> 4, cz = z >> 4, k = (cx & 1) | (cz & 1) << 1;
        Chunk c = near[k];
        if (c == null || nearX[k] != cx || nearZ[k] != cz) {
            c = Chunk.at(chunks, cx, cz);
            near[k] = c;
            nearX[k] = cx;
            nearZ[k] = cz;
        }
        return c.block(x, y, z);
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

    private int newLiquid(int x, int y, int z, int dropoff, boolean infinite) {
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
    public static int newLiquid(
            Tables t, ChunkIndex chunks, Object over, int x, int y, int z, int dropoff, boolean infinite) {
        return new Flow(t, chunks, over).newLiquid(x, y, z, dropoff, infinite);
    }

    private boolean replaceableDown(int st) {
        int f = t.fluid()[st];
        if (f == 0) return true;
        if (f == 1) return t.cls() != 1;
        return t.cls() == 1 && amount(st) / 9.0 >= 0.44444445;
    }

    /// Returns the level of the liquid that the liquid at `x` `y` `z`
    /// spreads down to, or -1 when it spreads not down.
    public static int downLevel(
            Tables t, ChunkIndex chunks, Object over, int x, int y, int z, int dropoff, boolean infinite) {
        return new Flow(t, chunks, over).down(x, y, z, dropoff, infinite);
    }

    /// Returns true when the liquid at `x` `y` `z` has a hole below.
    public static boolean hole(Tables t, ChunkIndex chunks, Object over, int x, int y, int z) {
        return new Flow(t, chunks, over).holeAt(x, y, z);
    }

    /// Returns true when lava is at one of the six cells around `x`
    /// `y` `z`.
    public static boolean lavaNear(Tables t, ChunkIndex chunks, Object over, int x, int y, int z) {
        return new Flow(t, chunks, over).lavaAround(x, y, z);
    }

    private int cell(int x, int z) {
        return (x - px + r) * w + (z - pz + r);
    }

    private int cellRaw(int i, int dy) {
        int k = i * SLOTS + (dy == 0 ? 0 : 1);
        long v = cells[k];
        if ((v & HI) == stamp) return (int) v;
        int s = raw(px - r + i / w, py + dy, pz - r + i % w);
        cells[k] = stamp | s;
        return s;
    }

    private boolean hole(int i) {
        int k = i * SLOTS + 2;
        long v = cells[k];
        if ((v & HI) == stamp) return (int) v == 1;
        int braw = cellRaw(i, -1);
        boolean h = pass(cellRaw(i, 0), braw, DOWN) && t.holeFloor()[braw];
        cells[k] = stamp | (h ? 1 : 0);
        return h;
    }

    private int step(int i, int d) {
        return i + DX[d] * w + DZ[d];
    }

    private boolean passable(int i, int d) {
        int k = i * SLOTS + 3 + d;
        long v = cells[k];
        if ((v & HI) == stamp) return (int) v == 1;
        int traw = cellRaw(i, 0);
        boolean ok = t.enterable()[traw] && pass(cellRaw(step(i, d ^ 1), 0), traw, d);
        cells[k] = stamp | (ok ? 1 : 0);
        return ok;
    }

    private int slopeDistance(int c, int back, long mark) {
        seen[c] = mark;
        queue[0] = c;
        int head = 0, tail = 1;
        for (int pass = 1; pass <= slope; pass++) {
            for (int end = tail; head < end; head++) {
                int i = queue[head];
                for (int d = 0; d < 4; d++) {
                    int n = step(i, d);
                    if (i == c && d == back || seen[n] == mark || !passable(n, d)) continue;
                    if (hole(n)) return pass;
                    seen[n] = mark;
                    queue[tail++] = n;
                }
            }
        }
        return 1000;
    }

    private boolean replaceable(int st) {
        int f = t.fluid()[st];
        if (f == 0) return true;
        if (f == t.cls()) return false;
        return t.cls() == 1 && amount(st) / 9.0 >= 0.44444445;
    }

    private boolean candidate(int raw, int d, int[] out, int dropoff, boolean infinite) {
        int tx = px + DX[d], tz = pz + DZ[d];
        int traw = cellRaw(cell(tx, tz), 0);
        if (source(traw) || !t.holdsAny()[traw] || !pass(raw, traw, d)) return false;
        int v = newLiquid(tx, py, tz, dropoff, infinite);
        if (v < 0) return false;
        boolean[] holds = v == 0 ? t.holdsSource() : t.holdsFlowing();
        if (!holds[traw]) return false;
        out[d] = v;
        return true;
    }

    private int side(int d) {
        return cell(px + DX[d], pz + DZ[d]);
    }

    private void begin(int x, int y, int z, int slope) {
        px = x;
        py = y;
        pz = z;
        this.slope = slope;
        r = slope + 1;
        w = 2 * r + 1;
        SlopeSearch sc = SEARCH.get();
        if (++sc.epoch == Integer.MAX_VALUE) {
            Arrays.fill(sc.cells, 0L);
            Arrays.fill(sc.seen, 0L);
            sc.epoch = 1;
        }
        stamp = sc.epoch << 32;
        int n = w * w;
        boolean fit = n <= sc.seen.length;
        cells = fit ? sc.cells : new long[SLOTS * n];
        seen = fit ? sc.seen : new long[n];
        queue = fit ? sc.queue : new int[n];
    }

    private void distances(int[] levels, int[] dist) {
        boolean holes = false;
        for (int d = 0; d < 4; d++) {
            dist[d] = levels[d] >= 0 && hole(side(d)) ? 0 : 1000;
            holes |= dist[d] == 0;
        }
        for (int d = 0; d < 4 && !holes; d++) {
            if (levels[d] < 0) continue;
            dist[d] = slopeDistance(side(d), d ^ 1, stamp | (d + 1));
        }
    }

    private int[] lowestTargets(int x, int y, int z, int dropoff, int slope, boolean infinite) {
        begin(x, y, z, slope);
        int raw = raw(x, y, z);
        int[] levels = {-1, -1, -1, -1};
        boolean[] fits = new boolean[4];
        int open = 0, fit = 0;
        for (int d = 0; d < 4; d++) {
            if (!candidate(raw, d, levels, dropoff, infinite)) continue;
            open++;
            fits[d] = replaceable(cellRaw(side(d), 0));
            if (fits[d]) fit++;
        }
        if (fit > 0 && open > 1) {
            int[] dist = new int[4];
            distances(levels, dist);
            int lowest = 1000;
            for (int d = 0; d < 4; d++) {
                if (levels[d] >= 0) lowest = Math.min(lowest, dist[d]);
            }
            for (int d = 0; d < 4; d++) {
                fits[d] = fits[d] && dist[d] == lowest;
            }
        }
        int[] out = new int[9];
        for (int d = 3; d >= 0; d--) {
            if (fits[d]) {
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
    public static int[] lowestTargets(
            Tables t, ChunkIndex chunks, Object over, int x, int y, int z, int dropoff, int slope, boolean infinite) {
        return new Flow(t, chunks, over).lowestTargets(x, y, z, dropoff, slope, infinite);
    }

    private void add(int x, int y, int z, int st, int dropped) {
        if (5 * mn + 6 > made.length) {
            made = Arrays.copyOf(made, 2 * made.length);
        }
        int k = 1 + 5 * mn++;
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
            if (t.fluid()[raw(x + DX[d], y + dy, z + DZ[d])] == 2) return true;
        }
        return false;
    }

    private int down(int x, int y, int z, int dropoff, boolean infinite) {
        int raw = raw(x, y, z), braw = raw(x, y - 1, z);
        if (source(braw) || !t.holdsAny()[braw] || !pass(raw, braw, DOWN)) return -1;
        int v = newLiquid(x, y - 1, z, dropoff, infinite);
        if (v < 0 || !replaceableDown(braw)) return -1;
        boolean[] holds = v == 0 ? t.holdsSource() : t.holdsFlowing();
        return holds[braw] ? v : -1;
    }

    private boolean holeAt(int x, int y, int z) {
        int braw = raw(x, y - 1, z);
        return pass(raw(x, y, z), braw, DOWN) && t.holeFloor()[braw];
    }

    private int sourcesAround(int x, int y, int z) {
        int n = 0;
        for (int d = 0; d < 4; d++) {
            if (source(raw(x + DX[d], y, z + DZ[d]))) n++;
        }
        return n;
    }

    private boolean sides(int x, int y, int z, int l, int dropoff, int slope, boolean infinite) {
        int n = l == 8 ? 7 : (l == 0 ? 8 : 8 - l) - dropoff;
        if (n <= 0) return true;
        int[] found = lowestTargets(x, y, z, dropoff, slope, infinite);
        for (int k = 0; k < found[0]; k++) {
            int d = found[1 + 2 * k];
            if (!spreadTo(x + DX[d], y, z + DZ[d], found[2 + 2 * k])) return false;
        }
        return true;
    }

    private boolean spread(int x, int y, int z, int l, int dropoff, int slope, boolean infinite) {
        int v = down(x, y, z, dropoff, infinite);
        if (v >= 0) {
            return spreadTo(x, y - 1, z, v)
                    && (sourcesAround(x, y, z) < 3 || sides(x, y, z, l, dropoff, slope, infinite));
        }
        if (l == 0 || !holeAt(x, y, z)) {
            return sides(x, y, z, l, dropoff, slope, infinite);
        }
        return true;
    }

    private int[] cell(int x, int y, int z, int dropoff, int slope, boolean infinite) {
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
        made[0] = mn;
        return made;
    }

    /// Returns the changes of the liquid at `x` `y` `z` on its fluid
    /// tick: their count, then x, y, z, the state and the state that
    /// the flow drops or -1 each. Returns null when a change asks for
    /// more: a container, a mix with lava or another liquid.
    public static int[] cell(
            Tables t, ChunkIndex chunks, int x, int y, int z, int dropoff, int slope, boolean infinite) {
        return new Flow(t, chunks, null).cell(x, y, z, dropoff, slope, infinite);
    }
}
