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
    private static final Keyword STUCK = Keyword.intern("stuck?");

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
        RECOMPUTE,
        STUCK
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

    private static final int[] NO_CELLS = {};
    private static final boolean[] NO_CUTS = {};

    /// The navigation of a mob that has never walked anywhere.
    public static final Nav FRESH = new Nav();

    private Object path;
    private long index;
    private Object target;
    private Object reach;
    private double speed;
    private long tick;
    private long stuckCheck;
    private Object stuckPos;
    private Object timeoutNode;
    private long timeoutTimer;
    private long timeoutCheck;
    private double timeoutLimit;
    private boolean delayed;
    private long recompute;
    private boolean stuck;
    private int[] xs, ys, zs;
    private boolean[] cuts;

    private Nav() {
        reach = 1L;
        stuckPos = PersistentVector.create(0.0, 0.0, 0.0);
        timeoutNode = ORIGIN;
        xs = ys = zs = NO_CELLS;
        cuts = NO_CUTS;
    }

    private Nav(Nav o) {
        path = o.path;
        index = o.index;
        target = o.target;
        reach = o.reach;
        speed = o.speed;
        tick = o.tick;
        stuckCheck = o.stuckCheck;
        stuckPos = o.stuckPos;
        timeoutNode = o.timeoutNode;
        timeoutTimer = o.timeoutTimer;
        timeoutCheck = o.timeoutCheck;
        timeoutLimit = o.timeoutLimit;
        delayed = o.delayed;
        recompute = o.recompute;
        stuck = o.stuck;
        xs = o.xs;
        ys = o.ys;
        zs = o.zs;
        cuts = o.cuts;
    }

    private void walk(Object p) {
        if (p == path) return;
        path = p;
        Object nodes = p == null ? null : RT.get(p, NODES);
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
        Nav n = new Nav(this);
        if (k == PATH) n.walk(v);
        else if (k == INDEX) n.index = lng(v);
        else if (k == TARGET) n.target = v;
        else if (k == REACH) n.reach = v;
        else if (k == SPEED) n.speed = dbl(v);
        else if (k == TICK) n.tick = lng(v);
        else if (k == STUCK_CHECK) n.stuckCheck = lng(v);
        else if (k == STUCK_POS) n.stuckPos = v;
        else if (k == TIMEOUT_NODE) n.timeoutNode = v;
        else if (k == TIMEOUT_TIMER) n.timeoutTimer = lng(v);
        else if (k == TIMEOUT_CHECK) n.timeoutCheck = lng(v);
        else if (k == TIMEOUT_LIMIT) n.timeoutLimit = dbl(v);
        else if (k == DELAYED) n.delayed = RT.booleanCast(v);
        else if (k == RECOMPUTE) n.recompute = lng(v);
        else if (k == STUCK) n.stuck = RT.booleanCast(v);
        else throw new IllegalArgumentException("no nav key " + k);
        return n;
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
            case 13 -> recompute;
            default -> stuck;
        };
    }

    private static int slot(Object k) {
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

    @Override
    public IPersistentMap assoc(Object k, Object v) {
        return slot(k) < 0 ? asMap().assoc(k, v) : with(k, v);
    }

    @Override
    public IPersistentMap assocEx(Object k, Object v) {
        return asMap().assocEx(k, v);
    }

    @Override
    public IPersistentMap without(Object k) {
        return asMap().without(k);
    }

    @Override
    public boolean containsKey(Object k) {
        return slot(k) >= 0;
    }

    @Override
    public IMapEntry entryAt(Object k) {
        int j = slot(k);
        return j < 0 ? null : MapEntry.create(k, at(j));
    }

    @Override
    public Object valAt(Object k) {
        return valAt(k, null);
    }

    @Override
    public Object valAt(Object k, Object notFound) {
        int j = slot(k);
        return j < 0 ? notFound : at(j);
    }

    @Override
    public int count() {
        return KEYS.length;
    }

    @Override
    public IPersistentCollection empty() {
        return PersistentArrayMap.EMPTY;
    }

    @Override
    public ISeq seq() {
        return asMap().seq();
    }

    @Override
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
        Nav n = new Nav(of(m));
        n.tick++;
        return n;
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
            Nav next = new Nav(n);
            next.index++;
            return next;
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
        Nav n = new Nav(this);
        if (advance(x, y, z, my, half)) n.index++;
        if (tick - stuckCheck > STUCK_INTERVAL) {
            double eff = drive >= 1.0 ? drive : drive * drive;
            double thr = eff * STUCK_INTERVAL * STUCK_FACTOR;
            double dx = x - RT.doubleCast(RT.nth(stuckPos, 0));
            double dy = my - RT.doubleCast(RT.nth(stuckPos, 1));
            double dz = z - RT.doubleCast(RT.nth(stuckPos, 2));
            n.stuckCheck = tick;
            n.stuckPos = PersistentVector.create(x, my, z);
            n.stuck = !(dx * dx + dy * dy + dz * dz >= thr * thr);
            if (n.stuck) n.walk(null);
        }
        if (n.path != null && n.index < xs.length) n.timed(drive, x, my, z, t);
        return n;
    }

    private void timed(double drive, double x, double my, double z, long t) {
        int j = (int) index;
        if (sameCell(timeoutNode, xs[j], ys[j], zs[j])) {
            timeoutTimer += t - timeoutCheck;
        } else {
            timeoutNode = PersistentVector.create(
                    (long) xs[j],
                    (long) ys[j],
                    (long) zs[j]
            );
            timeoutLimit = Walk.timeout(drive, x, my, z, xs[j], ys[j], zs[j]);
        }
        timeoutCheck = t;
        if (timeoutLimit > 0.0 && timeoutTimer > 3.0 * timeoutLimit) {
            walk(null);
            stuck = false;
            timeoutNode = ORIGIN;
            timeoutTimer = 0;
            timeoutLimit = 0.0;
        }
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
