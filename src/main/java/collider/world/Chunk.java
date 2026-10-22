package collider.world;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.Arrays;
import java.util.Objects;

/// A column of COUNT sections, some of them absent, with the light of
/// the sections just below and above it, which hold no blocks.
public final class Chunk {

    public static final int COUNT = 24;

    /// The index of the section that starts at y 0.
    public static final int OFFSET = 4;

    public static final int MIN_Y = -OFFSET * 16;

    public static final int MAX_Y = (COUNT - OFFSET) * 16 - 1;

    public static final int BELOW = -1;

    public static final int ABOVE = COUNT;

    public static final Chunk EMPTY = new Chunk(new Section[COUNT]);

    private static final int EDGES = 3;

    private final Section[] sections;
    private final byte[][] edges;
    private final Object token;

    private Chunk(Section[] sections) {
        this(sections, new byte[EDGES][], null);
    }

    private Chunk(Section[] sections, byte[][] edges, Object token) {
        this.sections = sections;
        this.edges = edges;
        this.token = token;
    }

    private static int edge(int si, int ch) {
        if (si == BELOW) {
            return ch == Light.SKY ? 0 : 1;
        }
        if (si == ABOVE && ch == Light.BLOCK) {
            return 2;
        }
        throw new IllegalArgumentException("edge " + si + " " + ch);
    }

    private static boolean dark(byte[] light) {
        for (byte b : light) {
            if (b != 0) {
                return false;
            }
        }
        return true;
    }

    /// Returns the light of channel ch in section si, BELOW or ABOVE
    /// the column, or null where it is dark. The sky light above is
    /// always full and is not kept.
    public byte[] edgeLight(int si, int ch) {
        return edges[edge(si, ch)];
    }

    /// Returns this chunk with the light of channel ch in section si,
    /// BELOW or ABOVE the column, set to light. Dark light is not kept.
    public Chunk withEdgeLight(int si, int ch, byte[] light) {
        int i = edge(si, ch);
        if (light != null && dark(light)) {
            light = null;
        }
        if (edges[i] == light) {
            return this;
        }
        byte[][] e = edges.clone();
        e[i] = light;
        return new Chunk(sections, e, null);
    }

    /// Returns the column of sections, from the lowest up, with the
    /// missing top ones absent.
    public static Chunk of(Object[] sections) {
        if (sections.length > COUNT) {
            throw new IllegalArgumentException("sections " + sections.length);
        }
        Section[] a = new Section[COUNT];
        for (int i = 0; i < sections.length; i++) {
            a[i] = (Section) sections[i];
        }
        return new Chunk(a);
    }

    /// Returns section si, or null when it is absent or outside the
    /// column.
    public Section section(int si) {
        return si >= 0 && si < COUNT ? sections[si] : null;
    }

    public Chunk with(int si, Section s) {
        if (sections[si] == s) {
            return this;
        }
        Section[] a = sections.clone();
        a[si] = s;
        return new Chunk(a, edges, null);
    }

    /// Returns this chunk with the block at x, y, z set to state, x and
    /// z taken within the chunk. The edit window token writes in place
    /// the chunk and the section it owns. A null token copies them.
    public Chunk withBlock(int x, int y, int z, int state, Object token) {
        int si = sectionIndex(y);
        Section s = sections[si];
        if (s == null) {
            s = fresh(si);
        }
        Section n = s.withOwned(Section.index(x, y, z), state, token);
        if (token == null) {
            return with(si, n);
        }
        if (this.token == token) {
            sections[si] = n;
            return this;
        }
        Section[] a = sections.clone();
        a[si] = n;
        return new Chunk(a, edges, token);
    }

    /// Returns this chunk with the kept signal at x, y, z set to v, x
    /// and z taken within the chunk. The edit window token writes in
    /// place the chunk it owns. A null token copies it.
    public Chunk withSignal(int x, int y, int z, int v, Object token) {
        int si = sectionIndex(y);
        Section s = sections[si];
        if (s == null) {
            s = fresh(si);
        }
        Section n = s.withSignal(Section.index(x, y, z), v, token);
        if (n == s) {
            return this;
        }
        if (token != null && this.token == token) {
            sections[si] = n;
            return this;
        }
        Section[] a = sections.clone();
        a[si] = n;
        return new Chunk(a, edges, token);
    }

    /// Returns the kept signal at x, y, z, x and z taken within the
    /// chunk. It is 0 outside the column and in an absent section.
    public int signal(int x, int y, int z) {
        Section s = section(sectionIndex(y));
        return s == null ? 0 : s.signal(Section.index(x, y, z));
    }

    /// Returns this chunk with the moving record at x, y, z set to
    /// packed, x and z taken within the chunk. A packed of 0 clears
    /// it. The edit window token writes in place the chunk it owns.
    /// A null token copies it.
    public Chunk withMoving(int x, int y, int z, long packed, Object token) {
        int si = sectionIndex(y);
        Section s = sections[si];
        if (s == null) {
            s = fresh(si);
        }
        Section n = s.withMoving(Section.index(x, y, z), packed, token);
        if (n == s) {
            return this;
        }
        if (token != null && this.token == token) {
            sections[si] = n;
            return this;
        }
        Section[] a = sections.clone();
        a[si] = n;
        return new Chunk(a, edges, token);
    }

    /// Returns the moving record at x, y, z, x and z taken within the
    /// chunk. It is 0 outside the column and in an absent section.
    public long moving(int x, int y, int z) {
        Section s = section(sectionIndex(y));
        return s == null ? 0L : s.moving(Section.index(x, y, z));
    }

    /// Returns the block state at x, y, z, x and z taken within the
    /// chunk. Air comes back outside the column and in an absent
    /// section.
    public int block(int x, int y, int z) {
        int si = sectionIndex(y);
        if (si < 0 || si >= COUNT) {
            return 0;
        }
        Section s = sections[si];
        if (s == null) {
            return 0;
        }
        return s.block(Section.index(x, y, z));
    }

    public static int sectionIndex(int y) {
        return (y >> 4) + OFFSET;
    }

    /// Returns the chunk at cx, cz in chunks, or EMPTY when absent.
    public static Chunk at(ChunkIndex chunks, int cx, int cz) {
        Object c = chunks.get(cx, cz);
        return c == null ? EMPTY : (Chunk) c;
    }

    /// Returns the block state at x, y, z in chunks. Air comes back
    /// outside the height and in an absent chunk or section.
    public static int blockAt(ChunkIndex chunks, int x, int y, int z) {
        return at(chunks, x >> 4, z >> 4).block(x, y, z);
    }

    /// Returns the kept signal at x, y, z in chunks, 0 in an absent
    /// chunk or section.
    public static int signalAt(ChunkIndex chunks, int x, int y, int z) {
        return at(chunks, x >> 4, z >> 4).signal(x, y, z);
    }

    /// Returns the moving record at x, y, z in chunks, 0 in an absent
    /// chunk or section.
    public static long movingAt(ChunkIndex chunks, int x, int y, int z) {
        return at(chunks, x >> 4, z >> 4).moving(x, y, z);
    }

    /// Returns the section that holds x, y, z in chunks, or null.
    public static Section sectionAt(ChunkIndex chunks, int x, int y, int z) {
        return at(chunks, x >> 4, z >> 4).section(sectionIndex(y));
    }

    /// Returns the lowest present section above section si, or null.
    public Section firstAbove(int si) {
        for (int i = Math.max(0, si + 1); i < COUNT; i++) {
            if (sections[i] != null) {
                return sections[i];
            }
        }
        return null;
    }

    /// Returns an air section for si with the sky light that the
    /// lowest present section above it lets down, or Section.EMPTY
    /// when there is none.
    public Section fresh(int si) {
        Section s = firstAbove(si);
        return s == null ? Section.EMPTY : s.below();
    }

    private int present() {
        int mask = 0;
        for (int i = 0; i < COUNT; i++) {
            if (sections[i] != null) {
                mask |= 1 << i;
            }
        }
        return mask;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (!(o instanceof Chunk c)) {
            return false;
        }
        for (int i = 0; i < COUNT; i++) {
            if (!Objects.equals(sections[i], c.sections[i])) {
                return false;
            }
        }
        return Arrays.deepEquals(edges, c.edges);
    }

    @Override
    public int hashCode() {
        int h = 1;
        for (Section s : sections) {
            h = 31 * h + Objects.hashCode(s);
        }
        return 31 * h + Arrays.deepHashCode(edges);
    }

    /// Writes the column to out in the snapshot form.
    public void save(DataOutput out) throws IOException {
        int mask = present();
        for (int i = 0; i < EDGES; i++) {
            if (edges[i] != null) {
                mask |= 1 << (COUNT + i);
            }
        }
        out.writeInt(mask);
        for (Section s : sections) {
            if (s != null) {
                s.save(out);
            }
        }
        for (byte[] e : edges) {
            if (e != null) {
                out.write(e);
            }
        }
    }

    /// Reads a column that save wrote.
    public static Chunk load(DataInput in) throws IOException {
        int mask = in.readInt();
        if ((mask >>> (COUNT + EDGES)) != 0) {
            throw new IOException("chunk sections " + mask);
        }
        Section[] a = new Section[COUNT];
        for (int i = 0; i < COUNT; i++) {
            if ((mask & (1 << i)) != 0) {
                a[i] = Section.load(in);
            }
        }
        byte[][] e = new byte[EDGES][];
        for (int i = 0; i < EDGES; i++) {
            if ((mask & (1 << (COUNT + i))) != 0) {
                e[i] = new byte[2048];
                in.readFully(e[i]);
            }
        }
        return new Chunk(a, e, null);
    }
}
