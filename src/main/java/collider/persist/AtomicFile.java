package collider.persist;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;

/// A file replaced whole. After a crash it holds the old bytes or the
/// new ones, never a mix.
public final class AtomicFile {

    private static final boolean WINDOWS =
            System.getProperty("os.name").startsWith("Windows");

    /// Replaces `target` with `data`. After a crash the file is the old
    /// one or the new one. The new one is durable on return.
    public static void put(Path target, byte[] data) throws IOException {
        Path dir = target.toAbsolutePath().getParent();
        Files.createDirectories(dir);
        Path tmp = target.resolveSibling(target.getFileName() + ".tmp");
        try (FileChannel c = FileChannel.open(
                tmp,
                StandardOpenOption.CREATE,
                StandardOpenOption.WRITE,
                StandardOpenOption.TRUNCATE_EXISTING
        )) {
            ByteBuffer b = ByteBuffer.wrap(data);
            while (b.hasRemaining()) {
                c.write(b);
            }
            c.force(true);
        } catch (IOException | RuntimeException e) {
            Files.deleteIfExists(tmp);
            throw e;
        }
        Files.move(
                tmp,
                target,
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING
        );
        sync(dir);
    }

    /// Makes the entries of directory `dir` durable. Windows cannot
    /// open a directory, and its file system journals the entries.
    private static void sync(Path dir) throws IOException {
        if (WINDOWS) return;
        try (FileChannel c = FileChannel.open(dir, StandardOpenOption.READ)) {
            c.force(true);
        }
    }
}
