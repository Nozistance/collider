package collider.persist;

import static java.nio.file.StandardOpenOption.*;

import collider.world.ChunkIndex;
import java.io.EOFException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.zip.CRC32;
import java.util.zip.DataFormatException;

/// The log of a region, 32 by 32 chunks, in one append-only file.
///
/// A record is `cx cz len crc32` as ints, then `len` bytes. The last
/// readable record of a chunk wins. Only the bytes up to `end` belong
/// to the region; a commit writes past them and a manifest names the
/// new end. A copy shares the file and owns its index, so readers of
/// the old region never see a record of an uncommitted write.
public final class Region {

    static final int HEAD = 16, SIDE = 32, ENTRY = 24;
    public final long key, gen;
    private final Log log;
    private final long[] at;
    private final int[] size;
    private long end, live;

    /// The file of a region, shared by its copies. An interrupt closes
    /// the channel under every thread; the next use opens it again.
    private static final class Log {
        final Path path;
        volatile FileChannel ch;
        private boolean shut;

        Log(Path path, FileChannel ch) {
            this.path = path;
            this.ch = ch;
        }

        synchronized FileChannel reopen(FileChannel dead) throws IOException {
            if (shut) throw new ClosedChannelException();
            if (ch == dead) ch = FileChannel.open(path, READ, WRITE);
            return ch;
        }

        synchronized void close() throws IOException {
            shut = true;
            ch.close();
        }
    }

    private interface Op<T> {
        T on(FileChannel c) throws IOException;
    }

    private <T> T io(Op<T> op) throws IOException {
        FileChannel c = log.ch;
        while (true) {
            try {
                return op.on(c);
            } catch (ClosedChannelException e) {
                if (Thread.currentThread().isInterrupted()) throw e;
                c = log.reopen(c);
            }
        }
    }

    private Region(Log log, long key, long gen, long[] at, int[] size, long end, long live) {
        this.log = log;
        this.key = key;
        this.gen = gen;
        this.at = at;
        this.size = size;
        this.end = end;
        this.live = live;
    }

    /// Returns the key of the region that holds chunk `id`.
    public static long of(long id) {
        return ChunkIndex.id((int) (id >> 32) >> 5, (int) id >> 5);
    }

    static Path file(Path dir, long key, long gen) {
        int rx = (int) (key >> 32), rz = (int) key;
        return dir.resolve("r." + rx + "." + rz + "." + gen + ".log");
    }

    static Path manifest(Path dir, long gen) {
        return dir.resolve("regions." + gen);
    }

    /// Returns a new empty region of generation `gen` in `dir`.
    public static Region create(Path dir, long key, long gen) throws IOException {
        Files.createDirectories(dir);
        Path p = file(dir, key, gen);
        FileChannel ch = FileChannel.open(p, CREATE, READ, WRITE, TRUNCATE_EXISTING);
        return new Region(new Log(p, ch), key, gen, new long[SIDE * SIDE], new int[SIDE * SIDE], 0, 0);
    }

    static Region open(Path dir, long key, long gen, long end) throws IOException {
        Path p = file(dir, key, gen);
        FileChannel ch = FileChannel.open(p, READ, WRITE);
        Region r = new Region(new Log(p, ch), key, gen, new long[SIDE * SIDE], new int[SIDE * SIDE], 0, 0);
        try {
            r.scan(end);
        } catch (IOException | RuntimeException e) {
            r.close();
            throw e;
        }
        return r;
    }

    private static int slot(long id) {
        return ((int) (id >> 32) & 31) + (((int) id & 31) << 5);
    }

    private long id(int slot) {
        int cx = ((int) (key >> 32) << 5) + (slot & 31);
        int cz = ((int) key << 5) + (slot >> 5);
        return ChunkIndex.id(cx, cz);
    }

    private void index(long id, long off, int n) {
        int s = slot(id);
        if (at[s] != 0) live -= size[s];
        at[s] = off + 1;
        size[s] = n;
        live += n;
        end = off + n;
    }

    private ByteBuffer read(long off, int n) throws IOException {
        ByteBuffer b = ByteBuffer.allocate(n);
        while (b.hasRemaining()) {
            if (io(c -> c.read(b, off + b.position())) < 0) {
                throw new EOFException(log.path + " ends at " + off);
            }
        }
        return b.flip();
    }

    private void scan(long limit) throws IOException {
        long stop = Math.min(limit, io(FileChannel::size));
        while (end + HEAD <= stop) {
            ByteBuffer h = read(end, HEAD);
            long id = ChunkIndex.id(h.getInt(0), h.getInt(4));
            int len = h.getInt(8);
            if (of(id) != key || len < 0 || end + HEAD + len > stop) {
                break;
            }
            index(id, end, HEAD + len);
        }
        end = stop;
    }

    /// Returns a region with its own index over the same file.
    public Region copy() {
        return new Region(log, key, gen, at.clone(), size.clone(), end, live);
    }

    private static int crc(byte[] data) {
        CRC32 c = new CRC32();
        c.update(data);
        return (int) c.getValue();
    }

    /// Writes chunk `id` past the end and makes it the chunk's record.
    public void append(long id, byte[] data) throws IOException {
        ByteBuffer b = ByteBuffer.allocate(HEAD + data.length);
        b.putInt((int) (id >> 32)).putInt((int) id).putInt(data.length);
        b.putInt(crc(data)).put(data).flip();
        long off = end;
        while (b.hasRemaining()) io(c -> c.write(b, off + b.position()));
        index(id, off, b.capacity());
    }

    private byte[] record(long off, long id) throws IOException {
        ByteBuffer h = read(off, HEAD);
        int len = h.getInt(8);
        if (ChunkIndex.id(h.getInt(0), h.getInt(4)) != id || len < 0 || off + HEAD + len > end) {
            return null;
        }
        byte[] data = read(off + HEAD, len).array();
        return crc(data) == h.getInt(12) ? data : null;
    }

    private byte[] older(long before, long id) throws IOException, DataFormatException {
        long[] offs = new long[8];
        int n = 0;
        for (long off = 0; off < before; ) {
            ByteBuffer h = read(off, HEAD);
            if (h.getInt(8) < 0) break;
            if (ChunkIndex.id(h.getInt(0), h.getInt(4)) == id) {
                if (n == offs.length) offs = Arrays.copyOf(offs, 2 * n);
                offs[n++] = off;
            }
            off += HEAD + h.getInt(8);
        }
        while (n > 0) {
            byte[] data = record(offs[--n], id);
            if (data != null) return data;
        }
        throw new DataFormatException("no record of it reads in " + log.path);
    }

    /// Returns the bytes of chunk `id`, or null when it has no record.
    /// Throws DataFormatException when no record of it reads.
    /// A record whose crc fails gives way to the one before it.
    public byte[] get(long id) throws IOException, DataFormatException {
        long a = at[slot(id)];
        if (a == 0) return null;
        byte[] data = record(a - 1, id);
        return data != null ? data : older(a - 1, id);
    }

    /// Returns the ids of the chunks the region holds.
    public long[] chunks() {
        int n = 0;
        long[] ids = new long[SIDE * SIDE];
        for (int s = 0; s < at.length; s++) if (at[s] != 0) ids[n++] = id(s);
        return Arrays.copyOf(ids, n);
    }

    /// Returns true when the file holds more than twice its live bytes.
    public boolean sparse() {
        return end > 2 * live;
    }

    /// Returns a new region of generation `gen` with the live records
    /// alone. A chunk that no record of reads is left out.
    public Region compact(long gen) throws IOException {
        Region r = create(log.path.getParent(), key, gen);
        try {
            for (long id : chunks()) {
                byte[] data;
                try {
                    data = get(id);
                } catch (DataFormatException e) {
                    continue;
                }
                r.append(id, data);
            }
        } catch (IOException | RuntimeException e) {
            r.close();
            throw e;
        }
        return r;
    }

    /// Makes the written records durable.
    public void force() throws IOException {
        io(c -> {
            c.force(false);
            return null;
        });
    }

    public void close() throws IOException {
        log.close();
    }

    /// Writes the manifest of generation `gen` that names `regions`
    /// with their ends.
    public static void manifest(Path dir, long gen, Region[] regions) throws IOException {
        ByteBuffer b = ByteBuffer.allocate(regions.length * ENTRY);
        for (Region r : regions) b.putLong(r.key).putLong(r.gen).putLong(r.end);
        put(manifest(dir, gen), b.array());
    }

    private static ByteBuffer entries(Path dir, long gen) throws IOException {
        ByteBuffer b = ByteBuffer.wrap(Files.readAllBytes(manifest(dir, gen)));
        if (b.remaining() % ENTRY != 0) {
            throw new IOException(manifest(dir, gen) + " is cut short");
        }
        return b;
    }

    /// Returns the regions the manifest of generation `gen` names, with
    /// their indexes read from the record heads.
    public static Region[] open(Path dir, long gen) throws IOException {
        if (gen == 0) return new Region[0];
        ByteBuffer b = entries(dir, gen);
        Region[] rs = new Region[b.remaining() / ENTRY];
        int i = 0;
        try {
            for (; i < rs.length; i++) {
                rs[i] = open(dir, b.getLong(), b.getLong(), b.getLong());
            }
        } catch (IOException | RuntimeException e) {
            while (i > 0) rs[--i].close();
            throw e;
        }
        return rs;
    }

    private static void keep(Set<Path> kept, Path dir, long gen) throws IOException {
        if (gen <= 0 || !Files.isRegularFile(manifest(dir, gen))) return;
        kept.add(manifest(dir, gen));
        ByteBuffer b = entries(dir, gen);
        while (b.hasRemaining()) {
            kept.add(file(dir, b.getLong(), b.getLong()));
            b.getLong();
        }
    }

    /// Deletes every region file and manifest in `dir` that neither
    /// generation `gen` nor the one before it names.
    public static void sweep(Path dir, long gen) throws IOException {
        if (!Files.isDirectory(dir)) return;
        Set<Path> kept = new HashSet<>();
        keep(kept, dir, gen);
        keep(kept, dir, gen - 1);
        try (DirectoryStream<Path> ps = Files.newDirectoryStream(dir)) {
            for (Path p : ps) {
                String n = p.getFileName().toString();
                boolean ours = n.startsWith("r.") || n.startsWith("regions.");
                if (ours && !kept.contains(p)) Files.delete(p);
            }
        }
    }

    /// Replaces `target` with `data` whole: a crash leaves either the
    /// old file or the new one, and the new one is durable on return.
    public static void put(Path target, byte[] data) throws IOException {
        Path dir = target.toAbsolutePath().getParent();
        Files.createDirectories(dir);
        Path tmp = target.resolveSibling(target.getFileName() + ".tmp");
        try (FileChannel c = FileChannel.open(tmp, CREATE, WRITE, TRUNCATE_EXISTING)) {
            ByteBuffer b = ByteBuffer.wrap(data);
            while (b.hasRemaining()) c.write(b);
            c.force(true);
        } catch (IOException | RuntimeException e) {
            Files.deleteIfExists(tmp);
            throw e;
        }
        Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        sync(dir);
    }

    private static final boolean WINDOWS = System.getProperty("os.name").startsWith("Windows");

    /// Makes the entries of directory `dir` durable. Windows cannot
    /// open a directory, and its file system journals the entries.
    public static void sync(Path dir) throws IOException {
        if (WINDOWS) return;
        try (FileChannel c = FileChannel.open(dir, READ)) {
            c.force(true);
        }
    }
}
