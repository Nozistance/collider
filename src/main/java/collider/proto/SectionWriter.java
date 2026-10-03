package collider.proto;

import collider.world.Section;

/// The network form of a chunk section and its light.
public final class SectionWriter {

    private static final byte[] DARK = new byte[2048];

    /// Writes section `s` to `buf`. It counts the blocks whose id is
    /// true in `fluid` and gives every block the `biome`.
    public static void write(Buf buf, Section s, boolean[] fluid, int biome) {
        long t = s.tally(fluid);
        buf.writeShort((int) (t >>> 16));
        buf.writeShort((int) (t & 0xFFFF));
        buf.writeByte(s.bits());
        writePalette(buf, s);
        for (int c = 0, n = s.wordCount(); c < n; c++) buf.writeLong(s.word(c));
        buf.writeByte(0);
        varint(buf, biome);
    }

    private static void writePalette(Buf buf, Section s) {
        int n = s.paletteSize();
        if (n == 0) return;
        if (s.bits() != 0) varint(buf, n);
        for (int k = 0; k < n; k++) varint(buf, s.paletteId(k));
    }

    private static void varint(Buf buf, int v) {
        while ((v & ~0x7F) != 0) {
            buf.writeByte((v & 0x7F) | 0x80);
            v >>>= 7;
        }
        buf.writeByte(v);
    }

    public static void writeBlockLight(Buf buf, Section s) {
        byte[] a = s.blockLightBytes();
        buf.writeBytes(a == null ? DARK : a);
    }

    public static void writeSkyLight(Buf buf, Section s) {
        byte[] a = s.skyLightBytes();
        buf.writeBytes(a == null ? DARK : a);
    }
}
