package collider.java;

import clojure.lang.IFn;
import clojure.lang.RT;
import java.util.Arrays;
import java.util.HashMap;

/// Block light and sky light: the levels stored in the sections, the
/// flood that spreads them after a change, and the brightness of the
/// sky over the day.
///
/// A flood works on a cache of light arrays, one for each section and
/// channel it touched, by the key `(chunk id << 6) | (section index <<
/// 1) | channel`. Channel 0 is block light and channel 1 sky light.
/// The block functions it takes are `dampening` and `emits`, which map
/// a block state to a level, and `occludes`, which tells whether the
/// faces of two states that meet along a direction seal.
public final class Light {

    /// The channel of sky light.
    public static final int SKY = 1;

    private static final int MIN_Y = -64;

    private static final int MAX_Y = 319;

    private static final long OFF = 8388608;

    private static final int DOWN = 0;

    private static final int[] DX = {0, 0, 0, 0, -1, 1};

    private static final int[] DY = {-1, 1, 0, 0, 0, 0};

    private static final int[] DZ = {0, 0, -1, 1, 0, 0};

    private static final int DAY = 24000;

    /// The times of day that bound the pieces of the sky level, one
    /// piece before the first and one after the last wrap the day.
    private static final long[] SEG_T = {-1670, 133, 11867, 13670, 22330,
                                         24133};

    private static final float DUSK = 0.26666668F;

    /// The sky level at each time of `SEG_T`.
    private static final float[] SEG_V = {DUSK, 1.0F, 1.0F, DUSK, DUSK,
                                          1.0F};

    private final HashMap<Long, byte[]> cache;
    private final ChunkIndex chunks;
    private final int ch;
    private final IFn.LL dampening;
    private final IFn.LL emits;
    private final IFn.LLLO occludes;
    private long[] rq = new long[64];
    private int rqHead, rqTail;
    private long[] pq = new long[64];
    private int pqHead, pqTail;

    private Light(HashMap<Long, byte[]> cache, ChunkIndex chunks, int ch,
            IFn.LL dampening, IFn.LL emits, IFn.LLLO occludes) {
        this.cache = cache;
        this.chunks = chunks;
        this.ch = ch;
        this.dampening = dampening;
        this.emits = emits;
        this.occludes = occludes;
    }

    private static boolean inRange(long y) {
        return MIN_Y <= y && y <= MAX_Y;
    }

    private static long pack(long x, long y, long z, long l) {
        return ((x + OFF) << 38) | ((z + OFF) << 14)
            | ((y - MIN_Y + 1) << 4) | l;
    }

    private static long px(long e) {
        return (e >> 38) - OFF;
    }

    private static long pz(long e) {
        return ((e >> 14) & 0xFFFFFF) - OFF;
    }

    private static long py(long e) {
        return MIN_Y + (((e >> 4) & 0x3FF) - 1);
    }

    private static int idx(long x, long y, long z) {
        return (int) ((y & 15) * 256 + (z & 15) * 16 + (x & 15));
    }

    private static long sectionIndex(long y) {
        return (y >> 4) + 4;
    }

    private static long key(long x, long y, long z, long ch) {
        long id = ((x >> 4) & 0xFFFFFFFFL) << 32 | ((z >> 4) & 0xFFFFFFFFL);
        return (id << 6) | (sectionIndex(y) << 1) | ch;
    }

    private static Section section(ChunkIndex chunks, long x, long y,
            long z) {
        return Chunk.sectionAt(chunks, (int) x, (int) y, (int) z);
    }

    private static long block(ChunkIndex chunks, long x, long y, long z) {
        return inRange(y) ? Chunk.blockAt(chunks, (int) x, (int) y,
                                          (int) z) : 0;
    }

    private static long absentSky(ChunkIndex chunks, long x, long y,
            long z) {
        Chunk c = Chunk.at(chunks, (int) (x >> 4), (int) (z >> 4));
        Section s = c.firstAbove((int) sectionIndex(y));
        return s == null ? 15 : s.skyLight((int) ((z & 15) * 16
                                                  + (x & 15)));
    }

    private static long outside(long ch, long y) {
        return ch == SKY && y > MAX_Y ? 15 : 0;
    }

    /// Returns the level of channel `ch` stored at `x`, `y`, `z` in
    /// `chunks`. Sky light is full above the world, and a missing
    /// section takes the sky light from above.
    public static long stored(ChunkIndex chunks, long ch, long x, long y,
            long z) {
        if (!inRange(y)) return outside(ch, y);
        Section s = section(chunks, x, y, z);
        if (s != null) {
            int i = idx(x, y, z);
            return ch == SKY ? s.skyLight(i) : s.blockLight(i);
        }
        return ch == SKY ? absentSky(chunks, x, y, z) : 0;
    }

    /// Returns the brighter of the sky and block light at `x`, `y`,
    /// `z` in `chunks`.
    public static long at(ChunkIndex chunks, long x, long y, long z) {
        if (!inRange(y)) return outside(SKY, y);
        Section s = section(chunks, x, y, z);
        if (s == null) return absentSky(chunks, x, y, z);
        int i = idx(x, y, z);
        return Math.max(s.skyLight(i), s.blockLight(i));
    }

    /// Returns the block light at `x`, `y`, `z` in `chunks`.
    public static long blockAt(ChunkIndex chunks, long x, long y,
            long z) {
        return stored(chunks, 0, x, y, z);
    }

    /// Returns the sky light at `x`, `y`, `z` in `chunks`.
    public static long skyAt(ChunkIndex chunks, long x, long y, long z) {
        return stored(chunks, SKY, x, y, z);
    }

    /// Returns the lowest y of the column `x`, `z` in `chunks` that
    /// the sky reaches straight down.
    public static long skySource(ChunkIndex chunks, long x, long z,
            IFn.LL dampening, IFn.LLLO occludes) {
        long top = 0;
        for (long y = MAX_Y; y >= MIN_Y; y--) {
            long b = block(chunks, x, y, z);
            if (dampening.invokePrim(b) > 0
                || RT.booleanCast(occludes.invokePrim(top, b, DOWN))) {
                return y + 1;
            }
            top = b;
        }
        return MIN_Y;
    }

    /// Spreads channel `ch` from the `cells` into `cache`. Each cell
    /// is `[x y z level]`: the old light of every cell goes dark,
    /// then each cell shines at its level and the light floods out.
    public static void pass(HashMap<Long, byte[]> cache,
            ChunkIndex chunks, long ch, Object cells, IFn.LL dampening,
            IFn.LL emits, IFn.LLLO occludes) {
        Light l = new Light(cache, chunks, (int) ch, dampening, emits,
                            occludes);
        for (Object c : (Iterable<?>) cells) l.clear(c);
        l.unlight();
        for (Object c : (Iterable<?>) cells) l.seed(c);
        l.propagate();
    }

    private static long nth(Object c, int i) {
        return RT.longCast(RT.nth(c, i));
    }

    private void addR(long e) {
        if (rqTail == rq.length) {
            rq = Arrays.copyOfRange(rq, rqHead, rqHead + 2 * rq.length);
            rqTail -= rqHead;
            rqHead = 0;
        }
        rq[rqTail++] = e;
    }

    private void addP(long e) {
        if (pqTail == pq.length) {
            pq = Arrays.copyOfRange(pq, pqHead, pqHead + 2 * pq.length);
            pqTail -= pqHead;
            pqHead = 0;
        }
        pq[pqTail++] = e;
    }

    private long get(long x, long y, long z) {
        if (!inRange(y)) return outside(ch, y);
        byte[] a = cache.get(key(x, y, z, ch));
        return a != null ? Section.nibble(a, idx(x, y, z))
                         : stored(chunks, ch, x, y, z);
    }

    private byte[] fresh(long x, long y, long z) {
        Section s = section(chunks, x, y, z);
        if (s == null) {
            Chunk c = Chunk.at(chunks, (int) (x >> 4), (int) (z >> 4));
            s = c.fresh((int) sectionIndex(y));
        }
        return ch == SKY ? s.skyLightCopy() : s.blockLightCopy();
    }

    private boolean set(long x, long y, long z, long v) {
        if (!inRange(y)) return false;
        long k = key(x, y, z, ch);
        byte[] a = cache.get(k);
        if (a == null) {
            a = fresh(x, y, z);
            cache.put(k, a);
        }
        Section.setNibble(a, idx(x, y, z), (int) v);
        return true;
    }

    private void clear(Object c) {
        long x = nth(c, 0), y = nth(c, 1), z = nth(c, 2);
        long cur = get(x, y, z);
        if (cur > 0) {
            set(x, y, z, 0);
            addR(pack(x, y, z, cur));
        }
    }

    private void seed(Object c) {
        long x = nth(c, 0), y = nth(c, 1), z = nth(c, 2);
        long source = nth(c, 3);
        if (source > 0 && source > get(x, y, z)) {
            set(x, y, z, source);
            addP(pack(x, y, z, source));
        }
        for (int d = 0; d < 6; d++) {
            long nx = x + DX[d], ny = y + DY[d], nz = z + DZ[d];
            long ln = get(nx, ny, nz);
            if (ln > 0) addP(pack(nx, ny, nz, ln));
        }
    }

    private void reEmit(long x, long y, long z) {
        long em = emits.invokePrim(block(chunks, x, y, z));
        if (em > 0 && ch == 0) {
            set(x, y, z, em);
            addP(pack(x, y, z, em));
        }
    }

    private void unlight() {
        while (rqHead < rqTail) {
            long e = rq[rqHead++];
            long l = e & 0xF;
            for (int d = 0; d < 6; d++) {
                long nx = px(e) + DX[d], ny = py(e) + DY[d];
                long nz = pz(e) + DZ[d];
                long ln = get(nx, ny, nz);
                if (ln <= 0) continue;
                if (ln >= l) {
                    addP(pack(nx, ny, nz, ln));
                } else if (set(nx, ny, nz, 0)) {
                    addR(pack(nx, ny, nz, ln));
                    reEmit(nx, ny, nz);
                }
            }
        }
    }

    private void propagate() {
        while (pqHead < pqTail) {
            long e = pq[pqHead++];
            long x = px(e), y = py(e), z = pz(e), l = e & 0xF;
            if (l != get(x, y, z)) continue;
            long from = block(chunks, x, y, z);
            for (int d = 0; d < 6; d++) {
                spread(from, x + DX[d], y + DY[d], z + DZ[d], l, d);
            }
        }
    }

    private void spread(long from, long nx, long ny, long nz, long l,
            int d) {
        if (!inRange(ny)) return;
        long to = block(chunks, nx, ny, nz);
        long cand = l - Math.max(1, dampening.invokePrim(to));
        if (cand > 0 && cand > get(nx, ny, nz)
            && !RT.booleanCast(occludes.invokePrim(from, to, d))
            && set(nx, ny, nz, cand)) {
            addP(pack(nx, ny, nz, cand));
        }
    }

    private static float skyFactor(long time) {
        long t = Math.floorMod(time, DAY);
        int i = 0;
        while (i < SEG_T.length - 2 && t >= SEG_T[i + 1]) i++;
        long t0 = SEG_T[i], t1 = SEG_T[i + 1];
        float v0 = SEG_V[i], v1 = SEG_V[i + 1];
        float a = (float) (t - t0) / (float) (t1 - t0);
        return v0 + a * (v1 - v0);
    }

    private static float blend(float v, float alpha, float target,
            float weight) {
        float to = v + alpha * (target - v);
        return v + weight * (to - v);
    }

    /// Returns the brightness of the sky, 0.0 to 15.0, at `time`
    /// of day. The `rain` and `thunder` levels, 0.0 to 1.0, dim it.
    public static double skyLevel(long time, double rain,
            double thunder) {
        float th = (float) thunder;
        float r = (float) rain - th;
        float v = 15.0F * skyFactor(time);
        if (r > 0) v = blend(v, 0.3125F, 4.0F, r);
        if (th > 0) v = blend(v, 0.52734375F, 4.0F, th);
        return Math.min(15.0F, Math.max(0.0F, v));
    }

    /// Returns how much the sky light is dimmed, 0 to 15, at `time`
    /// of day with the `rain` and `thunder` levels.
    public static long darken(long time, double rain, double thunder) {
        return (int) (15.0F - (float) skyLevel(time, rain, thunder));
    }

    /// Returns the light level at `x`, `y`, `z` in `chunks` at
    /// `time` of day with the `rain` and `thunder` levels.
    public static long brightness(ChunkIndex chunks, long x, long y,
            long z, long time, double rain, double thunder) {
        return Math.max(skyAt(chunks, x, y, z)
                        - darken(time, rain, thunder),
                        blockAt(chunks, x, y, z));
    }
}
