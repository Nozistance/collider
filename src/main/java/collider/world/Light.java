package collider.world;

import clojure.lang.RT;
import java.util.Arrays;

/// Block light and sky light, their flood after a change and the
/// brightness of the sky over the day.
public final class Light {

    public static final int BLOCK = 0;

    public static final int SKY = 1;

    private static final long COORD_BIAS = 8388608;

    private static final int DOWN = 0;

    private static final int[] DX = {0, 0, 0, 0, -1, 1};

    private static final int[] DY = {-1, 1, 0, 0, 0, 0};

    private static final int[] DZ = {0, 0, -1, 1, 0, 0};

    private static final int DAY = 24000;

    private static final long[] DAY_TIMES = {-1670, 133, 11867, 13670, 22330, 24133};

    private static final float DUSK = 0.26666668F;

    private static final float[] DAY_FACTORS = {DUSK, 1.0F, 1.0F, DUSK, DUSK, 1.0F};

    private static final long LOW = Chunk.MIN_Y - 16;

    private final Scratch<byte[]> cache;
    private final ChunkIndex chunks;
    private final int ch;
    private final BlockTables t;
    private final LongQueue decreases;
    private final LongQueue increases;
    private long lastKey = -1;
    private byte[] last;

    private Light(
            Scratch<byte[]> cache,
            ChunkIndex chunks,
            int ch,
            BlockTables t,
            LongQueue decreases,
            LongQueue increases
    ) {
        this.cache = cache;
        this.chunks = chunks;
        this.ch = ch;
        this.t = t;
        this.decreases = decreases;
        this.increases = increases;
    }

    private static boolean inRange(long y) {
        return Chunk.MIN_Y <= y && y <= Chunk.MAX_Y;
    }

    /// Returns true when channel ch keeps light at y, in the world or
    /// in the section below or above it. The sky above the world is
    /// always full and is not kept.
    private static boolean lit(long ch, long y) {
        return LOW <= y && y <= (ch == SKY ? Chunk.MAX_Y : Chunk.MAX_Y + 16);
    }

    private static long pack(long x, long y, long z, long l) {
        return ((x + COORD_BIAS) << 38)
                | ((z + COORD_BIAS) << 14)
                | ((y - LOW + 1) << 4)
                | l;
    }

    private static long px(long e) {
        return (e >> 38) - COORD_BIAS;
    }

    private static long pz(long e) {
        return ((e >> 14) & 0xFFFFFF) - COORD_BIAS;
    }

    private static long py(long e) {
        return LOW + (((e >> 4) & 0x3FF) - 1);
    }

    private static int idx(long x, long y, long z) {
        return Section.index((int) x, (int) y, (int) z);
    }

    private static long sectionIndex(long y) {
        return (y >> 4) + Chunk.OFFSET;
    }

    private static long key(long x, long y, long z, long ch) {
        long id = ((x >> 4) & 0xFFFFFFFFL) << 32 | ((z >> 4) & 0xFFFFFFFFL);
        return (id << 6) | ((sectionIndex(y) + 1) << 1) | ch;
    }

    private static Section section(ChunkIndex chunks, long x, long y, long z) {
        return Chunk.sectionAt(chunks, (int) x, (int) y, (int) z);
    }

    private static long block(ChunkIndex chunks, long x, long y, long z) {
        return inRange(y) ? Chunk.blockAt(chunks, (int) x, (int) y, (int) z) : 0;
    }

    private static long absentSky(ChunkIndex chunks, long x, long y, long z) {
        Chunk c = Chunk.at(chunks, (int) (x >> 4), (int) (z >> 4));
        Section s = c.firstAbove((int) sectionIndex(y));
        return s == null ? 15 : s.skyLight((int) ((z & 15) * 16 + (x & 15)));
    }

    private static long outside(long ch, long y) {
        return ch == SKY && y > Chunk.MAX_Y ? 15 : 0;
    }

    private static byte[] edge(ChunkIndex chunks, long ch, long x, long y, long z) {
        Chunk c = Chunk.at(chunks, (int) (x >> 4), (int) (z >> 4));
        return c.edgeLight((int) sectionIndex(y), (int) ch);
    }

    /// Returns the level of channel ch at x y z in chunks. Sky light is
    /// full above the world, and a missing section takes the sky light
    /// from above.
    private static long stored(ChunkIndex chunks, long ch, long x, long y, long z) {
        if (!lit(ch, y)) {
            return outside(ch, y);
        }
        if (!inRange(y)) {
            byte[] a = edge(chunks, ch, x, y, z);
            return a == null ? 0 : Section.nibble(a, idx(x, y, z));
        }
        Section s = section(chunks, x, y, z);
        if (s != null) {
            int i = idx(x, y, z);
            return ch == SKY ? s.skyLight(i) : s.blockLight(i);
        }
        return ch == SKY ? absentSky(chunks, x, y, z) : 0;
    }

    /// Returns the brighter of the sky and block light at x y z in
    /// chunks.
    public static long at(ChunkIndex chunks, long x, long y, long z) {
        if (!inRange(y)) {
            return outside(SKY, y);
        }
        Section s = section(chunks, x, y, z);
        if (s == null) {
            return absentSky(chunks, x, y, z);
        }
        int i = idx(x, y, z);
        return Math.max(s.skyLight(i), s.blockLight(i));
    }

    public static long blockAt(ChunkIndex chunks, long x, long y, long z) {
        return stored(chunks, BLOCK, x, y, z);
    }

    /// Returns the sky light at x y z in chunks, full above the world.
    public static long skyAt(ChunkIndex chunks, long x, long y, long z) {
        return stored(chunks, SKY, x, y, z);
    }

    /// Returns the lowest y of the column x z in chunks that the sky
    /// reaches straight down.
    private static long skySource(ChunkIndex chunks, long x, long z, BlockTables t) {
        long top = 0;
        for (long y = Chunk.MAX_Y; y >= Chunk.MIN_Y; y--) {
            long b = block(chunks, x, y, z);
            if (Block.dampening(t, b) > 0 || Block.occludes(t, top, b, DOWN)) {
                return y + 1;
            }
            top = b;
        }
        return LOW;
    }

    /// Returns chunks relit after changes [pos old new], with sky light
    /// only when sky is true. A chunk takes its higher sections first,
    /// so that a new section starts from the sky light above it.
    public static ChunkIndex relit(
            ChunkIndex chunks,
            Object changes,
            boolean sky,
            BlockTables t
    ) {
        Scratch<byte[]> cache = relight(chunks, changes, sky, t);
        if (cache == null || cache.isEmpty()) {
            return chunks;
        }
        long[] ks = cache.sortedKeys();
        long[] ids = new long[ks.length];
        Object[] vals = new Object[ks.length];
        int n = 0;
        for (int i = ks.length - 1; i >= 0; ) {
            long id = ks[i] >> 6;
            Chunk c = (Chunk) chunks.get(id);
            for (; i >= 0 && (ks[i] >> 6) == id; i--) {
                if (c == null) {
                    continue;
                }
                int si = (int) ((ks[i] >> 1) & 31) - 1;
                byte[] a = cache.get(ks[i]);
                int channel = (int) (ks[i] & 1);
                if (si == Chunk.BELOW || si == Chunk.ABOVE) {
                    c = c.withEdgeLight(si, channel, a);
                    continue;
                }
                Section s = c.section(si);
                if (s == null) {
                    s = c.fresh(si);
                }
                c = c.with(si, channel == SKY ? s.withSkyLight(a) : s.withBlockLight(a));
            }
            if (c != null) {
                ids[n] = id;
                vals[n++] = c;
            }
        }
        return chunks.withAll(Arrays.copyOf(ids, n), Arrays.copyOf(vals, n));
    }

    private static Scratch<byte[]> relight(
            ChunkIndex chunks,
            Object changes,
            boolean sky,
            BlockTables t
    ) {
        long[] cells = new long[RT.count(changes)];
        int n = 0;
        for (Object c : (Iterable<?>) changes) {
            long old = nth(c, 1);
            long now = nth(c, 2);
            if (!relightNeeded(t, old, now)) {
                continue;
            }
            Object p = RT.nth(c, 0);
            long x = nth(p, 0);
            long y = nth(p, 1);
            long z = nth(p, 2);
            cells[n++] = pack(x, y, z, Block.emission(t, block(chunks, x, y, z)));
        }
        if (n == 0) {
            return null;
        }
        Scratch<byte[]> cache = new Scratch<>();
        LongQueue dec = new LongQueue();
        LongQueue inc = new LongQueue();
        new Light(cache, chunks, BLOCK, t, dec, inc).pass(cells, n);
        if (sky) {
            long[] sc = skyCells(chunks, cells, n, t);
            new Light(cache, chunks, SKY, t, dec, inc).pass(sc, sc.length);
        }
        return cache;
    }

    private static boolean relightNeeded(BlockTables t, long old, long now) {
        return old != now
                && (Block.dampening(t, old) != Block.dampening(t, now)
                        || Block.emission(t, old) != Block.emission(t, now)
                        || Block.useShape(t, old)
                        || Block.useShape(t, now));
    }

    private void pass(long[] cells, int n) {
        for (int k = 0; k < n; k++) {
            clear(cells[k]);
        }
        unlight();
        for (int k = 0; k < n; k++) {
            seed(cells[k]);
        }
        propagate();
    }

    private static boolean skyFull(ChunkIndex chunks, long x, long y, long z) {
        return stored(chunks, SKY, x, y, z) == 15;
    }

    /// Returns the sky light seeds of the columns that cells stand in.
    /// Every cell between the old and the new lowest sky cell of a
    /// column is seeded before the changed cells, each once. A seed is
    /// full at or above the new lowest sky cell and dark below it.
    private static long[] skyCells(
            ChunkIndex chunks,
            long[] cells,
            int n,
            BlockTables t
    ) {
        long[] ps = new long[n];
        for (int k = 0; k < n; k++) {
            ps[k] = cells[k] & ~0xFL;
        }
        Arrays.sort(ps);
        long[] out = new long[n + 64];
        int m = 0;
        for (int a = 0; a < n; ) {
            int b = a;
            while (b < n && (ps[b] >> 14) == (ps[a] >> 14)) {
                b++;
            }
            long x = px(ps[a]);
            long z = pz(ps[a]);
            long src = skySource(chunks, x, z, t);
            long lo = src;
            long hi = src;
            while (lit(SKY, lo - 1) && skyFull(chunks, x, lo - 1, z)) {
                lo--;
            }
            while (hi <= Chunk.MAX_Y && !skyFull(chunks, x, hi, z)) {
                hi++;
            }
            int need = m + (int) (hi - lo) + (b - a);
            if (need > out.length) {
                out = Arrays.copyOf(out, Math.max(need, 2 * out.length));
            }
            for (long y = src - 1; y >= lo; y--) {
                out[m++] = pack(x, y, z, 0);
            }
            for (long y = src; y < hi; y++) {
                out[m++] = pack(x, y, z, 15);
            }
            long prev = Long.MIN_VALUE;
            for (int k = a; k < b; k++) {
                long y = py(ps[k]);
                if (y == prev || (lo <= y && y < hi)) {
                    continue;
                }
                prev = y;
                out[m++] = pack(x, y, z, y >= src ? 15 : 0);
            }
            a = b;
        }
        return Arrays.copyOf(out, m);
    }

    private static long nth(Object c, int i) {
        return RT.longCast(RT.nth(c, i));
    }

    private byte[] cached(long k) {
        if (k != lastKey) {
            last = cache.get(k);
            lastKey = k;
        }
        return last;
    }

    private long get(long x, long y, long z) {
        if (!lit(ch, y)) {
            return outside(ch, y);
        }
        byte[] a = cached(key(x, y, z, ch));
        return a != null ? Section.nibble(a, idx(x, y, z)) : stored(chunks, ch, x, y, z);
    }

    private byte[] fresh(long x, long y, long z) {
        if (!inRange(y)) {
            byte[] a = edge(chunks, ch, x, y, z);
            return a == null ? new byte[2048] : a.clone();
        }
        Section s = section(chunks, x, y, z);
        if (s == null) {
            Chunk c = Chunk.at(chunks, (int) (x >> 4), (int) (z >> 4));
            s = c.fresh((int) sectionIndex(y));
        }
        return ch == SKY ? s.skyLightCopy() : s.blockLightCopy();
    }

    private boolean set(long x, long y, long z, long v) {
        if (!lit(ch, y)) {
            return false;
        }
        long k = key(x, y, z, ch);
        byte[] a = cached(k);
        if (a == null) {
            a = fresh(x, y, z);
            cache.put(k, a);
            last = a;
        }
        Section.setNibble(a, idx(x, y, z), (int) v);
        return true;
    }

    private void clear(long c) {
        long x = px(c);
        long y = py(c);
        long z = pz(c);
        long cur = get(x, y, z);
        if (cur > 0) {
            set(x, y, z, 0);
            decreases.add(pack(x, y, z, cur));
        }
    }

    private void seed(long c) {
        long x = px(c);
        long y = py(c);
        long z = pz(c);
        long source = c & 0xF;
        if (source > 0 && source > get(x, y, z)) {
            set(x, y, z, source);
            increases.add(pack(x, y, z, source));
        }
        for (int d = 0; d < 6; d++) {
            long nx = x + DX[d];
            long ny = y + DY[d];
            long nz = z + DZ[d];
            long ln = get(nx, ny, nz);
            if (ln > 0) {
                increases.add(pack(nx, ny, nz, ln));
            }
        }
    }

    private void reEmit(long x, long y, long z) {
        if (ch != BLOCK) {
            return;
        }
        long em = Block.emission(t, block(chunks, x, y, z));
        if (em > 0) {
            set(x, y, z, em);
            increases.add(pack(x, y, z, em));
        }
    }

    private void unlight() {
        while (!decreases.isEmpty()) {
            long e = decreases.poll();
            long x = px(e);
            long y = py(e);
            long z = pz(e);
            long l = e & 0xF;
            for (int d = 0; d < 6; d++) {
                long nx = x + DX[d];
                long ny = y + DY[d];
                long nz = z + DZ[d];
                long ln = get(nx, ny, nz);
                if (ln <= 0) {
                    continue;
                }
                if (ln >= l) {
                    increases.add(pack(nx, ny, nz, ln));
                } else if (set(nx, ny, nz, 0)) {
                    decreases.add(pack(nx, ny, nz, ln));
                    reEmit(nx, ny, nz);
                }
            }
        }
    }

    private void propagate() {
        while (!increases.isEmpty()) {
            long e = increases.poll();
            long x = px(e);
            long y = py(e);
            long z = pz(e);
            long l = e & 0xF;
            if (l != get(x, y, z)) {
                continue;
            }
            long from = block(chunks, x, y, z);
            for (int d = 0; d < 6; d++) {
                spread(from, x + DX[d], y + DY[d], z + DZ[d], l, d);
            }
        }
    }

    private void spread(long from, long nx, long ny, long nz, long l, int d) {
        if (!lit(ch, ny)) {
            return;
        }
        long to = block(chunks, nx, ny, nz);
        long cand = l - Math.max(1, Block.dampening(t, to));
        if (cand > 0
                && cand > get(nx, ny, nz)
                && !Block.occludes(t, from, to, d)
                && set(nx, ny, nz, cand)) {
            increases.add(pack(nx, ny, nz, cand));
        }
    }

    private static float skyFactor(long time) {
        long t = Math.floorMod(time, DAY);
        int i = 0;
        while (i < DAY_TIMES.length - 2 && t >= DAY_TIMES[i + 1]) {
            i++;
        }
        long t0 = DAY_TIMES[i];
        long t1 = DAY_TIMES[i + 1];
        float v0 = DAY_FACTORS[i];
        float v1 = DAY_FACTORS[i + 1];
        float a = (float) (t - t0) / (float) (t1 - t0);
        return v0 + a * (v1 - v0);
    }

    private static float blend(float v, float alpha, float weight) {
        float to = v + alpha * (4.0F - v);
        return v + weight * (to - v);
    }

    /// Returns the sky brightness from 0 to 15 at time of day. The rain
    /// and thunder levels from 0 to 1 dim it.
    private static float skyLevel(long time, double rain, double thunder) {
        float th = (float) thunder;
        float r = (float) rain - th;
        float v = 15.0F * skyFactor(time);
        if (r > 0) {
            v = blend(v, 0.3125F, r);
        }
        if (th > 0) {
            v = blend(v, 0.52734375F, th);
        }
        return Math.clamp(v, 0.0F, 15.0F);
    }

    /// Returns how much the sky light is dimmed, 0 to 15, at time of
    /// day with the rain and thunder levels.
    public static long darken(long time, double rain, double thunder) {
        return (int) (15.0F - skyLevel(time, rain, thunder));
    }

    /// Returns the light level at x y z in chunks at time of day with
    /// the rain and thunder levels.
    public static long brightness(
            ChunkIndex chunks,
            long x,
            long y,
            long z,
            long time,
            double rain,
            double thunder
    ) {
        long sky = skyAt(chunks, x, y, z) - darken(time, rain, thunder);
        return Math.max(sky, blockAt(chunks, x, y, z));
    }
}
