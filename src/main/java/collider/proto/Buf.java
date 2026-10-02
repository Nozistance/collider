package collider.proto;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UTFDataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

/// A resizable byte sequence with a read position and a write position.
public final class Buf {

    private byte[] bytes;
    private int readIndex;
    private int writeIndex;

    public Buf(int capacity) {
        bytes = new byte[capacity];
    }

    private void checkRead(int n) {
        if (readIndex + n > writeIndex) {
            throw new IndexOutOfBoundsException(
                    "read " + n + " at " + readIndex + " of " + writeIndex
            );
        }
    }

    /// Makes room for `n` more bytes.
    public void ensure(int n) {
        if (writeIndex + n > bytes.length) {
            byte[] b = new byte[Math.max(bytes.length * 2, writeIndex + n)];
            System.arraycopy(bytes, 0, b, 0, writeIndex);
            bytes = b;
        }
    }

    /// Makes the first `len` bytes of `src` the unread content.
    public void adopt(byte[] src, int len) {
        bytes = src;
        readIndex = 0;
        writeIndex = len;
    }

    public int readableBytes() {
        return writeIndex - readIndex;
    }

    public void clear() {
        writeIndex = 0;
        readIndex = 0;
    }

    /// Empties this buffer and gives back the room above `keep` bytes.
    public void clear(int keep) {
        if (bytes.length > keep) bytes = new byte[keep];
        writeIndex = 0;
        readIndex = 0;
    }

    /// Returns a copy of up to `n` unread bytes. The read position stays.
    public byte[] peek(int n) {
        int k = Math.min(n, readableBytes());
        byte[] dst = new byte[k];
        System.arraycopy(bytes, readIndex, dst, 0, k);
        return dst;
    }

    public void readFrom(InputStream in, int n) throws IOException {
        ensure(n);
        int got = 0;
        while (got < n) {
            int k = in.read(bytes, writeIndex + got, n - got);
            if (k < 0) throw new IOException("end of stream");
            got += k;
        }
        writeIndex += n;
    }

    public void writeTo(OutputStream out) throws IOException {
        out.write(bytes, readIndex, writeIndex - readIndex);
        readIndex = writeIndex;
    }

    /// Gives the unread bytes to `inflater`. The read position stays.
    public void inflateInput(Inflater inflater) {
        inflater.setInput(bytes, readIndex, readableBytes());
    }

    /// Gives the unread bytes to `deflater` and marks them read.
    public void deflateInput(Deflater deflater) {
        deflater.setInput(bytes, readIndex, readableBytes());
        readIndex = writeIndex;
    }

    /// Writes what `deflater` gives into the free room.
    public void deflate(Deflater deflater) {
        writeIndex += deflater.deflate(bytes, writeIndex, bytes.length - writeIndex);
    }

    public void writeByte(int v) {
        ensure(1);
        bytes[writeIndex++] = (byte) v;
    }

    public void writeBoolean(boolean v) {
        writeByte(v ? 1 : 0);
    }

    public void writeShort(int v) {
        ensure(2);
        bytes[writeIndex++] = (byte) (v >> 8);
        bytes[writeIndex++] = (byte) v;
    }

    public void writeInt(int v) {
        ensure(4);
        for (int i = 24; i >= 0; i -= 8) bytes[writeIndex++] = (byte) (v >> i);
    }

    public void writeLong(long v) {
        ensure(8);
        for (int i = 56; i >= 0; i -= 8) bytes[writeIndex++] = (byte) (v >> i);
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
        ensure(len);
        System.arraycopy(src, off, bytes, writeIndex, len);
        writeIndex += len;
    }

    public void writeBytes(Buf src) {
        writeBytes(src.bytes, src.readIndex, src.readableBytes());
        src.readIndex = src.writeIndex;
    }

    private static int utfLength(String s) {
        int len = 0;
        for (int i = 0; i < s.length(); i++) {
            int ch = s.charAt(i);
            if (ch >= 0x0001 && ch <= 0x007F) len++;
            else if (ch > 0x07FF) len += 3;
            else len += 2;
        }
        return len;
    }

    /// Writes `s` in the modified UTF-8 of `DataOutput.writeUTF`.
    public void writeModifiedUtf(String s) throws UTFDataFormatException {
        int len = utfLength(s);
        if (len > 65535) {
            throw new UTFDataFormatException("encoded string too long: " + len + " bytes");
        }
        ensure(len + 2);
        bytes[writeIndex++] = (byte) (len >> 8);
        bytes[writeIndex++] = (byte) len;
        for (int i = 0; i < s.length(); i++) {
            int ch = s.charAt(i);
            if (ch >= 0x0001 && ch <= 0x007F) {
                bytes[writeIndex++] = (byte) ch;
            } else if (ch > 0x07FF) {
                bytes[writeIndex++] = (byte) (0xE0 | ((ch >> 12) & 0x0F));
                bytes[writeIndex++] = (byte) (0x80 | ((ch >> 6) & 0x3F));
                bytes[writeIndex++] = (byte) (0x80 | (ch & 0x3F));
            } else {
                bytes[writeIndex++] = (byte) (0xC0 | ((ch >> 6) & 0x1F));
                bytes[writeIndex++] = (byte) (0x80 | (ch & 0x3F));
            }
        }
    }

    private static UTFDataFormatException malformed(int at) {
        return new UTFDataFormatException("malformed input around byte " + at);
    }

    private static UTFDataFormatException partial() {
        return new UTFDataFormatException("malformed input: partial character at end");
    }

    /// Reads a string in the modified UTF-8 of `DataInput.readUTF`.
    public String readModifiedUtf() throws UTFDataFormatException {
        int len = readUnsignedShort();
        checkRead(len);
        int start = readIndex;
        readIndex += len;
        char[] chars = new char[len];
        int n = 0;
        int count = 0;
        while (count < len) {
            int c = bytes[start + count] & 0xFF;
            switch (c >> 4) {
                case 0, 1, 2, 3, 4, 5, 6, 7 -> {
                    count++;
                    chars[n++] = (char) c;
                }
                case 12, 13 -> {
                    count += 2;
                    if (count > len) throw partial();
                    int c2 = bytes[start + count - 1];
                    if ((c2 & 0xC0) != 0x80) throw malformed(count);
                    chars[n++] = (char) (((c & 0x1F) << 6) | (c2 & 0x3F));
                }
                case 14 -> {
                    count += 3;
                    if (count > len) throw partial();
                    int c2 = bytes[start + count - 2];
                    int c3 = bytes[start + count - 1];
                    if ((c2 & 0xC0) != 0x80 || (c3 & 0xC0) != 0x80) {
                        throw malformed(count - 1);
                    }
                    chars[n++] = (char) (((c & 0x0F) << 12) | ((c2 & 0x3F) << 6) | (c3 & 0x3F));
                }
                default -> throw malformed(count);
            }
        }
        return new String(chars, 0, n);
    }

    public byte readByte() {
        checkRead(1);
        return bytes[readIndex++];
    }

    public boolean readBoolean() {
        checkRead(1);
        return bytes[readIndex++] != 0;
    }

    public int readUnsignedByte() {
        checkRead(1);
        return bytes[readIndex++] & 0xFF;
    }

    public short readShort() {
        checkRead(2);
        return (short) (((bytes[readIndex++] & 0xFF) << 8) | (bytes[readIndex++] & 0xFF));
    }

    public int readUnsignedShort() {
        return readShort() & 0xFFFF;
    }

    public int readInt() {
        checkRead(4);
        int v = 0;
        for (int i = 0; i < 4; i++) v = (v << 8) | (bytes[readIndex++] & 0xFF);
        return v;
    }

    public long readLong() {
        checkRead(8);
        long v = 0;
        for (int i = 0; i < 8; i++) v = (v << 8) | (bytes[readIndex++] & 0xFF);
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
        System.arraycopy(bytes, readIndex, dst, 0, dst.length);
        readIndex += dst.length;
    }
}
