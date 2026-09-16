package collider.java;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.Arrays;

public final class Section {
    public static final int GLOBAL_BITS = 15;
    private static final int SIZE = 4096;
    private static final int LIGHT = 2048;
    private static final int MAX_PALETTE = 256;
    private static final byte[] ZERO = new byte[LIGHT];
    private static final byte[] FULL = filled();
    public static final Section EMPTY =
        new Section(0, new int[] {0}, null, null, FULL);

    private final int bits;
    private final int per;
    private final long mul;
    private final int[] pal;
    private final long[] data;
    private final byte[] bl;
    private final byte[] sl;

    private Section(int bits, int[] pal, long[] data, byte[] bl,
                    byte[] sl) {
        this.bits = bits;
        this.per = bits == 0 ? 0 : 64 / bits;
        this.mul = bits == 0 ? 0 : ((1L << 32) + per - 1) / per;
        this.pal = pal;
        this.data = data;
        this.bl = bl;
        this.sl = sl;
    }

    private static byte[] filled() {
        byte[] a = new byte[LIGHT];
        Arrays.fill(a, (byte) 0xFF);
        return a;
    }

    private static byte[] canon(byte[] a) {
        if (a == null || a == FULL) return a;
        if (a.length != LIGHT) {
            throw new IllegalArgumentException("light " + a.length);
        }
        if (Arrays.equals(a, ZERO)) return null;
        return Arrays.equals(a, FULL) ? FULL : a;
    }

    private static Section single(int state, byte[] bl, byte[] sl) {
        return new Section(0, new int[] {state}, null, bl, sl);
    }

    private static int width(int n) {
        if (n <= 1) return 0;
        if (n > MAX_PALETTE) return GLOBAL_BITS;
        int b = 32 - Integer.numberOfLeadingZeros(n - 1);
        return Math.max(4, b);
    }

    private static int longs(int bits) {
        if (bits == 0) return 0;
        int per = 64 / bits;
        return (SIZE + per - 1) / per;
    }

    public static Section of(short[] blocks, byte[] blockLight,
                             byte[] skyLight) {
        if (blocks.length != SIZE) {
            throw new IllegalArgumentException("blocks");
        }
        int[] v = new int[SIZE];
        for (int i = 0; i < SIZE; i++) v[i] = blocks[i] & 0xFFFF;
        return build(v, canon(blockLight), canon(skyLight));
    }

    private static Section build(int[] v, byte[] bl, byte[] sl) {
        int[] ids = new int[MAX_PALETTE + 1];
        int[] ix = new int[SIZE];
        int n = scan(v, ids, ix);
        int bits = width(n);
        if (bits == 0) return single(v[0], bl, sl);
        if (bits == GLOBAL_BITS) {
            return new Section(bits, null, pack(bits, v), bl, sl);
        }
        int[] p = Arrays.copyOf(ids, n);
        return new Section(bits, p, pack(bits, ix), bl, sl);
    }

    private static int scan(int[] v, int[] ids, int[] ix) {
        int[] slots = new int[1024];
        Arrays.fill(slots, -1);
        int n = 0;
        for (int i = 0; i < SIZE; i++) {
            int h = probe(slots, ids, v[i]);
            if (slots[h] < 0) {
                if (n == MAX_PALETTE) return n + 1;
                slots[h] = n;
                ids[n++] = v[i];
            }
            ix[i] = slots[h];
        }
        return n;
    }

    private static int probe(int[] slots, int[] ids, int id) {
        int h = (id * 0x9E3779B1) >>> 22;
        while (slots[h] >= 0 && ids[slots[h]] != id) {
            h = (h + 1) & 1023;
        }
        return h;
    }

    private static long[] pack(int bits, int[] v) {
        int per = 64 / bits;
        long[] out = new long[longs(bits)];
        for (int c = 0, i = 0; i < SIZE; c++) {
            long acc = 0;
            int end = Math.min(SIZE, i + per);
            for (int off = 0; i < end; i++, off += bits) {
                acc |= ((long) v[i]) << off;
            }
            out[c] = acc;
        }
        return out;
    }

    public int block(int i) {
        if (bits == 0) return pal[0];
        int c = (int) ((i * mul) >>> 32);
        int q = (int) ((data[c] >>> ((i - c * per) * bits))
                       & ((1L << bits) - 1));
        return pal == null ? q : pal[q];
    }

    private int index(long[] d, int i) {
        int c = (int) ((i * mul) >>> 32);
        return (int) ((d[c] >>> ((i - c * per) * bits))
                      & ((1L << bits) - 1));
    }

    private void put(long[] d, int i, int q) {
        if (bits == 0) return;
        int c = (int) ((i * mul) >>> 32);
        int off = (i - c * per) * bits;
        long mask = ((1L << bits) - 1) << off;
        d[c] = (d[c] & ~mask) | (((long) q) << off);
    }

    public int blockLight(int i) {
        return bl == null ? 0 : (bl[i >> 1] >> ((i & 1) << 2)) & 15;
    }

    public int skyLight(int i) {
        return sl == null ? 0 : (sl[i >> 1] >> ((i & 1) << 2)) & 15;
    }

    public int bits() {
        return bits;
    }

    public int paletteSize() {
        return pal == null ? 0 : pal.length;
    }

    public short[] blocks() {
        short[] out = new short[SIZE];
        for (int i = 0; i < SIZE; i++) out[i] = (short) block(i);
        return out;
    }

    public byte[] blockLightCopy() {
        return bl == null ? new byte[LIGHT] : bl.clone();
    }

    public byte[] skyLightCopy() {
        return sl == null ? new byte[LIGHT] : sl.clone();
    }

    public boolean hasBlockLight() {
        return bl != null;
    }

    public boolean hasSkyLight() {
        return sl != null;
    }

    public Section withBlockLight(byte[] a) {
        return new Section(bits, pal, data, canon(a), sl);
    }

    public Section withSkyLight(byte[] a) {
        return new Section(bits, pal, data, bl, canon(a));
    }

    public Section below() {
        if (sl == null || sl == FULL) {
            return single(0, null, sl);
        }
        byte[] a = new byte[LIGHT];
        for (int k = 0; k < 16; k++) {
            System.arraycopy(sl, 0, a, k * 128, 128);
        }
        return single(0, null, canon(a));
    }

    public Section with(int i, int state) {
        return apply(new int[] {i}, new int[] {state}, 1);
    }

    public Section apply(int[] idx, int[] states, int n) {
        if (n == 0) return this;
        long[] d = data == null ? null : data.clone();
        return run(idx, states, 0, n, d, pal);
    }

    private Section run(int[] idx, int[] st, int from, int n,
                        long[] d, int[] p) {
        for (int k = from; k < n; k++) {
            int q = find(p, st[k]);
            if (q < 0 && p.length == 1 << bits) {
                Section w = widen(d, p, st[k]);
                return w.run(idx, st, k, n, w.data, w.pal);
            }
            if (q < 0) {
                p = Arrays.copyOf(p, p.length + 1);
                q = p.length - 1;
                p[q] = st[k];
            }
            put(d, idx[k], q);
        }
        return settle(d, p);
    }

    private static int find(int[] p, int state) {
        if (p == null) return state;
        for (int k = 0; k < p.length; k++) {
            if (p[k] == state) return k;
        }
        return -1;
    }

    private static int[] kept(int[] p, int[] remap, int used,
                              int state) {
        int[] np = new int[used + 1];
        for (int q = 0; q < p.length; q++) {
            if (remap[q] >= 0) np[remap[q]] = p[q];
        }
        np[used] = state;
        return np;
    }

    private Section widen(long[] d, int[] p, int state) {
        int[] remap = new int[p.length];
        int used = used(d, p.length, remap);
        int[] np = kept(p, remap, used, state);
        int nb = width(used + 1);
        int[] v = new int[SIZE];
        for (int i = 0; i < SIZE; i++) {
            int q = remap[bits == 0 ? 0 : index(d, i)];
            v[i] = nb == GLOBAL_BITS ? np[q] : q;
        }
        int[] kept = nb == GLOBAL_BITS ? null : np;
        return new Section(nb, kept, pack(nb, v), bl, sl);
    }

    private int used(long[] d, int size, int[] remap) {
        Arrays.fill(remap, -1);
        if (bits == 0) {
            remap[0] = 0;
            return 1;
        }
        int n = 0;
        for (int i = 0; i < SIZE; i++) {
            int q = index(d, i);
            if (remap[q] < 0) remap[q] = n++;
        }
        return n;
    }

    private Section settle(long[] d, int[] p) {
        if (bits == 0) return new Section(0, p, null, bl, sl);
        if (p == null) return settleGlobal(d);
        int q0 = index(d, 0);
        if (uniform(d, q0)) {
            return single(p[q0], bl, sl);
        }
        return new Section(bits, p, d, bl, sl);
    }

    private boolean uniform(long[] d, int q) {
        long word = 0;
        for (int j = 0; j < per; j++) {
            word |= ((long) q) << (j * bits);
        }
        int full = SIZE / per;
        for (int c = 0; c < full; c++) {
            if (d[c] != word) return false;
        }
        int rest = SIZE - full * per;
        long mask = rest == 0 ? 0 : (1L << (rest * bits)) - 1;
        return rest == 0 || d[full] == (word & mask);
    }

    private Section settleGlobal(long[] d) {
        int[] v = new int[SIZE];
        for (int i = 0; i < SIZE; i++) v[i] = index(d, i);
        int[] ids = new int[MAX_PALETTE + 1];
        int[] ix = new int[SIZE];
        if (scan(v, ids, ix) > MAX_PALETTE) {
            return new Section(bits, null, d, bl, sl);
        }
        return build(v, bl, sl);
    }

    public boolean holds(boolean[] pred) {
        if (pal == null) return true;
        for (int id : pal) {
            if (id < pred.length && pred[id]) return true;
        }
        return false;
    }

    public void heights(boolean[] pred, int[] out, int base) {
        if (!holds(pred)) return;
        boolean[] hit = hits(pred);
        for (int c = 0; c < 256; c++) {
            if (out[c] == 0) out[c] = top(hit, c, base);
        }
    }

    private boolean[] hits(boolean[] pred) {
        if (pal == null) return pred;
        boolean[] hit = new boolean[pal.length];
        for (int q = 0; q < pal.length; q++) {
            hit[q] = pal[q] < pred.length && pred[pal[q]];
        }
        return hit;
    }

    private int top(boolean[] hit, int c, int base) {
        for (int y = 15; y >= 0; y--) {
            int i = (y << 8) | c;
            int q = bits == 0 ? 0 : index(data, i);
            if (q < hit.length && hit[q]) return base + y + 1;
        }
        return 0;
    }

    public void write(Buf buf, boolean[] fluid, int biome) {
        long t = tally(fluid);
        buf.writeShort((int) (t >>> 16));
        buf.writeShort((int) (t & 0xFFFF));
        buf.writeByte(bits);
        if (pal != null) writePalette(buf);
        if (data != null) {
            for (long x : data) buf.writeLong(x);
        }
        buf.writeByte(0);
        varint(buf, biome);
    }

    private void writePalette(Buf buf) {
        if (bits != 0) varint(buf, pal.length);
        for (int id : pal) varint(buf, id);
    }

    private static void varint(Buf buf, int v) {
        while ((v & ~0x7F) != 0) {
            buf.writeByte((v & 0x7F) | 0x80);
            v >>>= 7;
        }
        buf.writeByte(v);
    }

    private static boolean fluid(boolean[] fluid, int id) {
        return id < fluid.length && fluid[id];
    }

    private long tally(boolean[] fluid) {
        if (bits == 0) {
            int v = pal[0];
            long air = v == 0 ? 0 : SIZE;
            return (air << 16) | (fluid(fluid, v) ? SIZE : 0);
        }
        if (pal != null && !marked(fluid)) return (long) SIZE << 16;
        int[] counts = counts();
        long nonAir = 0, fl = 0;
        for (int q = 0; q < counts.length; q++) {
            int v = pal == null ? q : pal[q];
            if (v != 0) nonAir += counts[q];
            if (fluid(fluid, v)) fl += counts[q];
        }
        return (nonAir << 16) | fl;
    }

    private int[] counts() {
        int[] counts = new int[pal == null ? 1 << bits : pal.length];
        long mask = (1L << bits) - 1;
        for (int c = 0, i = 0; i < SIZE; c++) {
            long word = data[c];
            int end = Math.min(SIZE, i + per);
            for (; i < end; i++, word >>>= bits) {
                counts[(int) (word & mask)]++;
            }
        }
        return counts;
    }

    private boolean marked(boolean[] fluid) {
        for (int id : pal) {
            if (id == 0 || fluid(fluid, id)) return true;
        }
        return false;
    }

    public void writeBlockLight(Buf buf) {
        buf.writeBytes(bl == null ? ZERO : bl);
    }

    public void writeSkyLight(Buf buf) {
        buf.writeBytes(sl == null ? ZERO : sl);
    }

    public static void writeFullLight(Buf buf) {
        buf.writeBytes(FULL);
    }

    public void save(DataOutput out) throws IOException {
        out.writeByte(bits);
        if (pal != null) {
            out.writeShort(pal.length);
            for (int id : pal) out.writeInt(id);
        }
        if (data != null) {
            for (long x : data) out.writeLong(x);
        }
        saveLight(out, bl);
        saveLight(out, sl);
    }

    private static void saveLight(DataOutput out, byte[] a)
            throws IOException {
        out.writeByte(a == null ? 0 : a == FULL ? 1 : 2);
        if (a != null && a != FULL) out.write(a);
    }

    public static Section load(DataInput in) throws IOException {
        int bits = in.readUnsignedByte();
        boolean global = bits == GLOBAL_BITS;
        if (bits != 0 && !global && (bits < 4 || bits > 8)) {
            throw new IOException("section bits " + bits);
        }
        int[] pal = global ? null : loadPalette(in, bits);
        long[] data = new long[longs(bits)];
        for (int c = 0; c < data.length; c++) data[c] = in.readLong();
        byte[] bl = loadLight(in);
        byte[] sl = loadLight(in);
        long[] d = bits == 0 ? null : data;
        return new Section(bits, pal, d, bl, sl);
    }

    private static int[] loadPalette(DataInput in, int bits)
            throws IOException {
        int n = in.readUnsignedShort();
        int cap = bits == 0 ? 1 : 1 << bits;
        if (n < 1 || n > cap) {
            throw new IOException("section palette " + n);
        }
        int[] pal = new int[n];
        for (int k = 0; k < n; k++) pal[k] = in.readInt();
        return pal;
    }

    private static byte[] loadLight(DataInput in) throws IOException {
        int tag = in.readUnsignedByte();
        if (tag == 0) return null;
        if (tag == 1) return FULL;
        byte[] a = new byte[LIGHT];
        in.readFully(a);
        return canon(a);
    }
}
