package collider.persist;

import static java.nio.file.StandardOpenOption.CREATE;
import static java.nio.file.StandardOpenOption.READ;
import static java.nio.file.StandardOpenOption.TRUNCATE_EXISTING;
import static java.nio.file.StandardOpenOption.WRITE;

import collider.world.ChunkIndex;
import java.io.EOFException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.zip.CRC32;
import java.util.zip.DataFormatException;

/// The log of a region, 32 by 32 chunks, in one append-only file.
///
/// A record is `cx cz len crc32` as ints, then `len` bytes. The last
/// readable record of a chunk wins. Only the bytes up to `end` belong
/// to the region. A commit writes past them, and a manifest names the
/// new end. A copy shares the file and owns its index, so readers of
/// the old region never see a record of an uncommitted write.
public final class Region {

    static final int HEAD = 16;
    static final int SIDE = 32;
    static final int ENTRY = 24;
    public final long key;
    public final long gen;
    private final Log log;
    private final long[] at;
    private final int[] size;
    private long end;
    private long live;

    /// The file of a region, shared by its copies. An interrupt closes
    /// the channel for every user, and the next use opens it again.
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

    private Region(
            Log log,
            long key,
            long gen,
            long[] at,
            int[] size,
            long end,
            long live
    ) {
        this.log = log;
        this.key = key;
        this.gen = gen;
        this.at = at;
        this.size = size;
        this.end = end;
        this.live = live;
    }

    private static Region empty(Log log, long key, long gen) {
        int n = SIDE * SIDE;
        return new Region(log, key, gen, new long[n], new int[n], 0, 0);
    }

    /// Returns the key of the region that holds chunk `id`.
    public static long of(long id) {
        return ChunkIndex.id(ChunkIndex.x(id) >> 5, ChunkIndex.z(id) >> 5);
    }

    static Path file(Path dir, long key, long gen) {
        String xz = ChunkIndex.x(key) + "." + ChunkIndex.z(key);
        return dir.resolve("r." + xz + "." + gen + ".log");
    }

    static Path manifestPath(Path dir, long gen) {
        return dir.resolve("regions." + gen);
    }

    /// Returns a new empty region of generation `gen` in `dir`.
    public static Region create(Path dir, long key, long gen) throws IOException {
        Files.createDirectories(dir);
        Path p = file(dir, key, gen);
        FileChannel ch = FileChannel.open(p, CREATE, READ, WRITE, TRUNCATE_EXISTING);
        return empty(new Log(p, ch), key, gen);
    }

    static Region openOne(Path dir, long key, long gen, long end) throws IOException {
        Path p = file(dir, key, gen);
        FileChannel ch = FileChannel.open(p, READ, WRITE);
        Region r = empty(new Log(p, ch), key, gen);
        try {
            r.scan(end);
        } catch (IOException | RuntimeException e) {
            r.close();
            throw e;
        }
        return r;
    }

    private static int slot(long id) {
        return (ChunkIndex.x(id) & 31) + ((ChunkIndex.z(id) & 31) << 5);
    }

    private long id(int slot) {
        int cx = (ChunkIndex.x(key) << 5) + (slot & 31);
        int cz = (ChunkIndex.z(key) << 5) + (slot >> 5);
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
        b.putInt(ChunkIndex.x(id)).putInt(ChunkIndex.z(id)).putInt(data.length);
        b.putInt(crc(data)).put(data).flip();
        long off = end;
        while (b.hasRemaining()) io(c -> c.write(b, off + b.position()));
        index(id, off, b.capacity());
    }

    private byte[] record(long off, long id) throws IOException {
        ByteBuffer h = read(off, HEAD);
        int len = h.getInt(8);
        boolean ours = ChunkIndex.id(h.getInt(0), h.getInt(4)) == id;
        if (!ours || len < 0 || off + HEAD + len > end) {
            return null;
        }
        byte[] data = read(off + HEAD, len).array();
        return crc(data) == h.getInt(12) ? data : null;
    }

    private byte[] older(long before, long id) throws IOException, DataFormatException {
        long[] offs = new long[8];
        int n = 0;
        long off = 0;
        while (off < before) {
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
                } catch (DataFormatException _) {
                    continue;
                }
                r.append(id, Objects.requireNonNull(data));
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
    public static void writeManifest(Path dir, long gen, Region[] regions)
            throws IOException {
        ByteBuffer b = ByteBuffer.allocate(regions.length * ENTRY);
        for (Region r : regions) b.putLong(r.key).putLong(r.gen).putLong(r.end);
        AtomicFile.put(manifestPath(dir, gen), b.array());
    }

    private static ByteBuffer entries(Path dir, long gen) throws IOException {
        ByteBuffer b = ByteBuffer.wrap(Files.readAllBytes(manifestPath(dir, gen)));
        if (b.remaining() % ENTRY != 0) {
            throw new IOException(manifestPath(dir, gen) + " is cut short");
        }
        return b;
    }

    /// Returns the regions the manifest of generation `gen` names, each
    /// with its index.
    public static Region[] openAll(Path dir, long gen) throws IOException {
        if (gen == 0) return new Region[0];
        ByteBuffer b = entries(dir, gen);
        Region[] rs = new Region[b.remaining() / ENTRY];
        int i = 0;
        try {
            for (; i < rs.length; i++) {
                rs[i] = openOne(dir, b.getLong(), b.getLong(), b.getLong());
            }
        } catch (IOException | RuntimeException e) {
            while (i > 0) rs[--i].close();
            throw e;
        }
        return rs;
    }

    private static void keep(Set<Path> kept, Path dir, long gen) throws IOException {
        if (gen <= 0 || !Files.isRegularFile(manifestPath(dir, gen))) return;
        kept.add(manifestPath(dir, gen));
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
}
