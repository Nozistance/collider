package collider.game.mob;

import clojure.lang.APersistentMap;
import clojure.lang.IMapEntry;
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.ISeq;
import clojure.lang.Keyword;
import clojure.lang.MapEntry;
import clojure.lang.PersistentArrayMap;
import clojure.lang.PersistentVector;
import clojure.lang.RT;
import collider.world.Block;
import collider.world.BlockTables;
import collider.world.Chunk;
import collider.world.ChunkIndex;
import collider.world.space.Path;
import java.util.Iterator;

/// The ground navigation of a mob. It holds the path the mob walks
/// and the node it walks to, and drops a path the mob no longer gets
/// on with. It reads and changes as a map.
public final class Nav extends APersistentMap {

    private static final Keyword PATH = Keyword.intern("path");
    private static final Keyword INDEX = Keyword.intern("index");
    private static final Keyword TARGET = Keyword.intern("target");
    private static final Keyword REACH = Keyword.intern("reach");
    private static final Keyword SPEED = Keyword.intern("speed");
    private static final Keyword TICK = Keyword.intern("tick");
    private static final Keyword STUCK_CHECK = Keyword.intern("stuck-check");
    private static final Keyword STUCK_POS = Keyword.intern("stuck-pos");
    private static final Keyword TIMEOUT_NODE = Keyword.intern("timeout-node");
    private static final Keyword TIMEOUT_TIMER = Keyword.intern("timeout-timer");
    private static final Keyword TIMEOUT_CHECK = Keyword.intern("timeout-check");
    private static final Keyword TIMEOUT_LIMIT = Keyword.intern("timeout-limit");
    private static final Keyword DELAYED = Keyword.intern("delayed?");
    private static final Keyword RECOMPUTE = Keyword.intern("recompute");

    private static final Keyword[] KEYS = {
        PATH,
        INDEX,
        TARGET,
        REACH,
        SPEED,
        TICK,
        STUCK_CHECK,
        STUCK_POS,
        TIMEOUT_NODE,
        TIMEOUT_TIMER,
        TIMEOUT_CHECK,
        TIMEOUT_LIMIT,
        DELAYED,
        RECOMPUTE
    };

    private static final Keyword NODES = Keyword.intern("nodes");
    private static final Keyword X = Keyword.intern("x");
    private static final Keyword Y = Keyword.intern("y");
    private static final Keyword Z = Keyword.intern("z");
    private static final Keyword TYPE = Keyword.intern("type");
    private static final Keyword WATER = Keyword.intern("water");

    private static final Object ORIGIN = PersistentVector.create(0L, 0L, 0L);

    private static final int STUCK_INTERVAL = 100;
    private static final double STUCK_FACTOR = 0.25;
    private static final int MAX_SURFACE_STEPS = 16;

    /// The navigation of a mob that has never walked anywhere.
    public static final Nav FRESH = new Nav(
            null,
            0,
            null,
            1L,
            0.0,
            0,
            0,
            PersistentVector.create(0.0, 0.0, 0.0),
            ORIGIN,
            0,
            0,
            0.0,
            false,
            0
    );

    private final Object path;
    private final long index;
    private final Object target;
    private final Object reach;
    private final double speed;
    private final long tick;
    private final long stuckCheck;
    private final Object stuckPos;
    private final Object timeoutNode;
    private final long timeoutTimer;
    private final long timeoutCheck;
    private final double timeoutLimit;
    private final boolean delayed;
    private final long recompute;
    private final int[] xs, ys, zs;
    private final boolean[] cuts;

    private Nav(
            Object path,
            long index,
            Object target,
            Object reach,
            double speed,
            long tick,
            long stuckCheck,
            Object stuckPos,
            Object timeoutNode,
            long timeoutTimer,
            long timeoutCheck,
            double timeoutLimit,
            boolean delayed,
            long recompute) {
        this(
                path,
                index,
                target,
                reach,
                speed,
                tick,
                stuckCheck,
                stuckPos,
                timeoutNode,
                timeoutTimer,
                timeoutCheck,
                timeoutLimit,
                delayed,
                recompute,
                null);
    }

    private Nav(
            Object path,
            long index,
            Object target,
            Object reach,
            double speed,
            long tick,
            long stuckCheck,
            Object stuckPos,
            Object timeoutNode,
            long timeoutTimer,
            long timeoutCheck,
            double timeoutLimit,
            boolean delayed,
            long recompute,
            Nav same) {
        this.path = path;
        this.index = index;
        this.target = target;
        this.reach = reach;
        this.speed = speed;
        this.tick = tick;
        this.stuckCheck = stuckCheck;
        this.stuckPos = stuckPos;
        this.timeoutNode = timeoutNode;
        this.timeoutTimer = timeoutTimer;
        this.timeoutCheck = timeoutCheck;
        this.timeoutLimit = timeoutLimit;
        this.delayed = delayed;
        this.recompute = recompute;
        if (same != null && same.path == path) {
            xs = same.xs;
            ys = same.ys;
            zs = same.zs;
            cuts = same.cuts;
            return;
        }
        Object nodes = path == null ? null : RT.get(path, NODES);
        int n = RT.count(nodes);
        xs = new int[n];
        ys = new int[n];
        zs = new int[n];
        cuts = new boolean[n];
        for (int i = 0; i < n; i++) {
            Object node = RT.nth(nodes, i);
            xs[i] = RT.intCast(RT.get(node, X));
            ys[i] = RT.intCast(RT.get(node, Y));
            zs[i] = RT.intCast(RT.get(node, Z));
            cuts[i] = cutCorner(RT.get(node, TYPE));
        }
    }

    /// Returns true when a node of type `t` may be walked past on a
    /// corner.
    public static boolean cutCorner(Object t) {
        String s = t instanceof Keyword k ? k.getName() : "";
        return !(s.equals("fire-in-neighbor")
                || s.equals("damaging-in-neighbor")
                || s.equals("walkable-door"));
    }

    /// Returns the navigation `m` as a Nav, from a map when it is one.
    public static Nav of(Object m) {
        if (m instanceof Nav n) return n;
        if (m == null) return FRESH;
        Nav n = FRESH;
        for (ISeq s = RT.seq(m); s != null; s = s.next()) {
            IMapEntry e = (IMapEntry) s.first();
            n = n.with(e.key(), e.val());
        }
        return n;
    }

    private static long lng(Object v) {
        return v == null ? 0L : ((Number) v).longValue();
    }

    private static double dbl(Object v) {
        return v == null ? 0.0 : ((Number) v).doubleValue();
    }

    private Nav with(Object k, Object v) {
        Object p = path, t = target, r = reach, sp = stuckPos;
        Object tn = timeoutNode;
        long i = index, tk = tick, sc = stuckCheck, tt = timeoutTimer;
        long tc = timeoutCheck, rc = recompute;
        double s = speed, tl = timeoutLimit;
        boolean d = delayed;
        if (k == PATH) p = v;
        else if (k == INDEX) i = lng(v);
        else if (k == TARGET) t = v;
        else if (k == REACH) r = v;
        else if (k == SPEED) s = dbl(v);
        else if (k == TICK) tk = lng(v);
        else if (k == STUCK_CHECK) sc = lng(v);
        else if (k == STUCK_POS) sp = v;
        else if (k == TIMEOUT_NODE) tn = v;
        else if (k == TIMEOUT_TIMER) tt = lng(v);
        else if (k == TIMEOUT_CHECK) tc = lng(v);
        else if (k == TIMEOUT_LIMIT) tl = dbl(v);
        else if (k == DELAYED) d = RT.booleanCast(v);
        else if (k == RECOMPUTE) rc = lng(v);
        else throw new IllegalArgumentException("no nav key " + k);
        return new Nav(p, i, t, r, s, tk, sc, sp, tn, tt, tc, tl, d, rc, this);
    }

    private Object at(int j) {
        return switch (j) {
            case 0 -> path;
            case 1 -> index;
            case 2 -> target;
            case 3 -> reach;
            case 4 -> speed;
            case 5 -> tick;
            case 6 -> stuckCheck;
            case 7 -> stuckPos;
            case 8 -> timeoutNode;
            case 9 -> timeoutTimer;
            case 10 -> timeoutCheck;
            case 11 -> timeoutLimit;
            case 12 -> delayed;
            default -> recompute;
        };
    }

    private int slot(Object k) {
        for (int j = 0; j < KEYS.length; j++) {
            if (KEYS[j] == k) return j;
        }
        return -1;
    }

    private IPersistentMap asMap() {
        Object[] kvs = new Object[KEYS.length * 2];
        for (int j = 0; j < KEYS.length; j++) {
            kvs[2 * j] = KEYS[j];
            kvs[2 * j + 1] = at(j);
        }
        return new PersistentArrayMap(kvs);
    }

    public IPersistentMap assoc(Object k, Object v) {
        return slot(k) < 0 ? asMap().assoc(k, v) : with(k, v);
    }

    public IPersistentMap assocEx(Object k, Object v) {
        return asMap().assocEx(k, v);
    }

    public IPersistentMap without(Object k) {
        return asMap().without(k);
    }

    public boolean containsKey(Object k) {
        return slot(k) >= 0;
    }

    public IMapEntry entryAt(Object k) {
        int j = slot(k);
        return j < 0 ? null : MapEntry.create(k, at(j));
    }

    public Object valAt(Object k) {
        return valAt(k, null);
    }

    public Object valAt(Object k, Object notFound) {
        int j = slot(k);
        return j < 0 ? notFound : at(j);
    }

    public int count() {
        return KEYS.length;
    }

    public IPersistentCollection empty() {
        return PersistentArrayMap.EMPTY;
    }

    public ISeq seq() {
        return asMap().seq();
    }

    @SuppressWarnings("unchecked")
    public Iterator<Object> iterator() {
        return asMap().iterator();
    }

    /// Returns true when `m` has no path node left to walk to.
    public static boolean walked(Object m) {
        if (m == null) return true;
        Nav n = of(m);
        return n.path == null || n.index >= n.xs.length;
    }

    /// Returns `m` a tick older when it walks a path.
    public static Object ticked(Object m) {
        if (m == null || RT.get(m, PATH) == null) return m;
        Nav n = of(m);
        return n.with(TICK, n.tick + 1);
    }

    private static boolean water(ChunkIndex c, BlockTables t, int x, int y, int z) {
        return Block.name(t, Chunk.blockAt(c, x, y, z)) == WATER;
    }

    /// Returns the height a mob at `x`, `y`, `z` walks its path
    /// from. A wet mob walks from the water surface above it.
    public static double surfaceY(
            ChunkIndex c,
            BlockTables t,
            double x,
            double y,
            double z,
            boolean wet
    ) {
        if (!wet) return Math.floor(y + 0.5);
        int cx = (int) Math.floor(x), cz = (int) Math.floor(z);
        int y0 = (int) Math.floor(y);
        int cy = y0;
        for (int steps = 0; ; steps++) {
            if (!water(c, t, cx, cy, cz)) return cy;
            if (steps + 1 > MAX_SURFACE_STEPS) return y0;
            cy++;
        }
    }

    private static double offset(double half) {
        return 0.5 * (long) (2.0 * half + 1.0);
    }

    /// Returns the navigation after one step of the walk of a mob at
    /// `x`, `y`, `z`, `half` wide each side, driving at `drive`, on
    /// game tick `t`. The mob stands on ground or floats when `held`.
    public static Object stepped(
            Object m,
            ChunkIndex c,
            BlockTables tb,
            double x,
            double y,
            double z,
            boolean onGround,
            boolean wet,
            boolean held,
            double half,
            double drive,
            long t) {
        Nav n = of(m);
        double my = surfaceY(c, tb, x, y, z, wet);
        if (held) return n.follow(x, y, z, my, half, drive, t);
        int i = (int) n.index;
        double py = n.ys[i];
        if (my > py
                && !onGround
                && (long) Math.floor(x) == (long) Math.floor(n.xs[i] + offset(half))
                && (long) Math.floor(z) == (long) Math.floor(n.zs[i] + offset(half))) {
            return n.with(INDEX, n.index + 1);
        }
        return m;
    }

    private boolean advance(double x, double y, double z, double my, double half) {
        int i = (int) index;
        if (Walk.closeEnough(x, y, z, xs[i], ys[i], zs[i], 2.0 * half)) {
            return true;
        }
        return cuts[i]
                && i + 1 < xs.length
                && Walk.turnedBack(
                        x,
                        my,
                        z,
                        xs[i],
                        ys[i],
                        zs[i],
                        xs[i + 1],
                        ys[i + 1],
                        zs[i + 1]
                );
    }

    private static boolean sameCell(Object c, int x, int y, int z) {
        return RT.longCast(RT.nth(c, 0)) == x
                && RT.longCast(RT.nth(c, 1)) == y
                && RT.longCast(RT.nth(c, 2)) == z;
    }

    private Nav follow(
            double x,
            double y,
            double z,
            double my,
            double half,
            double drive,
            long t
    ) {
        long i = advance(x, y, z, my, half) ? index + 1 : index;
        Object p = path, sp = stuckPos, tn = timeoutNode;
        long sc = stuckCheck, tt = timeoutTimer, tc = timeoutCheck;
        double tl = timeoutLimit;
        if (tick - stuckCheck > STUCK_INTERVAL) {
            double eff = drive >= 1.0 ? drive : drive * drive;
            double thr = eff * STUCK_INTERVAL * STUCK_FACTOR;
            double dx = x - RT.doubleCast(RT.nth(stuckPos, 0));
            double dy = my - RT.doubleCast(RT.nth(stuckPos, 1));
            double dz = z - RT.doubleCast(RT.nth(stuckPos, 2));
            sc = tick;
            sp = PersistentVector.create(x, my, z);
            if (!(dx * dx + dy * dy + dz * dz >= thr * thr)) p = null;
        }
        if (p != null && i < xs.length) {
            int j = (int) i;
            if (sameCell(tn, xs[j], ys[j], zs[j])) {
                tt = tt + (t - tc);
            } else {
                tn = PersistentVector.create((long) xs[j], (long) ys[j], (long) zs[j]);
                tl = Walk.timeout(drive, x, my, z, xs[j], ys[j], zs[j]);
            }
            tc = t;
            if (tl > 0.0 && tt > 3.0 * tl) {
                p = null;
                tn = ORIGIN;
                tt = 0;
                tl = 0.0;
            }
        }
        return new Nav(
                p,
                i,
                target,
                reach,
                speed,
                tick,
                sc,
                sp,
                tn,
                tt,
                tc,
                tl,
                delayed,
                recompute,
                this
        );
    }

    /// Returns the move control `move` told to walk to the node the
    /// navigation `m` walks to, for a mob `half` wide each side.
    public static Steer aimed(
            Object m,
            Object move,
            ChunkIndex c,
            Object[] shapes,
            double half
    ) {
        Nav n = of(m);
        int i = (int) n.index;
        double off = offset(half);
        double x = n.xs[i] + off, y = n.ys[i], z = n.zs[i] + off;
        int cx = (int) Math.floor(x), cy = (int) Math.floor(y);
        int cz = (int) Math.floor(z);
        int below = Chunk.blockAt(c, cx, cy - 1, cz);
        double gy = below == 0 ? y : (cy - 1) + Path.shapeTop(shapes, below);
        return Steer.wanted(move, x, gy, z, n.speed);
    }
}
