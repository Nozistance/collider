package collider.java;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public final class Buf {
    public byte[] a;
    public int w, r;

    public Buf(int capacity) {
        a = new byte[capacity];
    }

    private void checkRead(int n) {
        if (r + n > w) {
            throw new IndexOutOfBoundsException("read " + n + " at " + r + " of " + w);
        }
    }

    private void need(int n) {
        if (w + n > a.length) {
            byte[] b = new byte[Math.max(a.length * 2, w + n)];
            System.arraycopy(a, 0, b, 0, w);
            a = b;
        }
    }

    public int readableBytes() {
        return w - r;
    }

    public void clear() {
        w = 0;
        r = 0;
    }

    public void clear(int keep) {
        if (a.length > keep) a = new byte[keep];
        w = 0;
        r = 0;
    }

    public void readFrom(InputStream in, int n) throws IOException {
        need(n);
        int got = 0;
        while (got < n) {
            int k = in.read(a, w + got, n - got);
            if (k < 0) throw new IOException("end of stream");
            got += k;
        }
        w += n;
    }

    public void writeTo(OutputStream out) throws IOException {
        out.write(a, r, w - r);
        r = w;
    }

    public void writeByte(int v) {
        need(1);
        a[w++] = (byte) v;
    }

    public void writeBoolean(boolean v) {
        writeByte(v ? 1 : 0);
    }

    public void writeShort(int v) {
        need(2);
        a[w++] = (byte) (v >> 8);
        a[w++] = (byte) v;
    }

    public void writeInt(int v) {
        need(4);
        for (int i = 24; i >= 0; i -= 8) a[w++] = (byte) (v >> i);
    }

    public void writeLong(long v) {
        need(8);
        for (int i = 56; i >= 0; i -= 8) a[w++] = (byte) (v >> i);
    }

    public void writeFloat(float v) {
        writeInt(Float.floatToIntBits(v));
    }

    public void writeDouble(double v) {
        writeLong(Double.doubleToLongBits(v));
    }

    public void writeBytes(byte[] src) {
        writeBytes(src, 0, src.length);
    }

    public void writeBytes(byte[] src, int off, int len) {
        need(len);
        System.arraycopy(src, off, a, w, len);
        w += len;
    }

    public void writeBytes(Buf src) {
        writeBytes(src.a, src.r, src.readableBytes());
        src.r = src.w;
    }

    public byte readByte() {
        checkRead(1);
        return a[r++];
    }

    public boolean readBoolean() {
        checkRead(1);
        return a[r++] != 0;
    }

    public int readUnsignedByte() {
        checkRead(1);
        return a[r++] & 0xFF;
    }

    public short readShort() {
        checkRead(2);
        return (short) (((a[r++] & 0xFF) << 8) | (a[r++] & 0xFF));
    }

    public int readUnsignedShort() {
        return readShort() & 0xFFFF;
    }

    public int readInt() {
        checkRead(4);
        int v = 0;
        for (int i = 0; i < 4; i++) v = (v << 8) | (a[r++] & 0xFF);
        return v;
    }

    public long readLong() {
        checkRead(8);
        long v = 0;
        for (int i = 0; i < 8; i++) v = (v << 8) | (a[r++] & 0xFF);
        return v;
    }

    public float readFloat() {
        return Float.intBitsToFloat(readInt());
    }

    public double readDouble() {
        return Double.longBitsToDouble(readLong());
    }

    public void readBytes(byte[] dst) {
        checkRead(dst.length);
        System.arraycopy(a, r, dst, 0, dst.length);
        r += dst.length;
    }
}
