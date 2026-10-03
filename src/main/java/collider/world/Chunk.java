package collider.world;

import java.io.DataInput;
import java.io.DataOutput;
import java.io.IOException;
import java.util.Objects;

/// A chunk column of `COUNT` sections, some of which may be absent.
public final class Chunk {

    public static final int COUNT = 24;

    /// The index of the section that starts at y 0.
    public static final int OFFSET = 4;

    public static final int MIN_Y = -OFFSET * 16;

    public static final int MAX_Y = (COUNT - OFFSET) * 16 - 1;

    public static final Chunk EMPTY = new Chunk(new Section[COUNT]);

    private final Section[] sections;
    private final Object token;

    private Chunk(Section[] sections) {
        this(sections, null);
    }

    private Chunk(Section[] sections, Object token) {
        this.sections = sections;
        this.token = token;
    }

    /// Returns the column of `sections`, from the lowest up, the
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

    public Section section(int si) {
        return si >= 0 && si < COUNT ? sections[si] : null;
    }

    public Chunk with(int si, Section s) {
        if (sections[si] == s) return this;
        Section[] a = sections.clone();
        a[si] = s;
        return new Chunk(a);
    }

    /// Returns this chunk with the block at chunk-relative x and z and
    /// world y set to `state`, written in place where the edit window
    /// `token` owns the chunk and its section, as `ChunkIndex` allows.
    public Chunk withBlock(int x, int y, int z, int state, Object token) {
        int si = sectionIndex(y);
        Section s = sections[si];
        if (s == null) s = fresh(si);
        Section n = s.withOwned(Section.index(x, y, z), state, token);
        if (token == null) return with(si, n);
        if (this.token == token) {
            sections[si] = n;
            return this;
        }
        Section[] a = sections.clone();
        a[si] = n;
        return new Chunk(a, token);
    }

    /// Returns the block state at chunk-relative x and z and world y.
    public int block(int x, int y, int z) {
        int si = sectionIndex(y);
        if (si < 0 || si >= COUNT) return 0;
        Section s = sections[si];
        if (s == null) return 0;
        return s.block(Section.index(x, y, z));
    }

    public static int sectionIndex(int y) {
        return (y >> 4) + OFFSET;
    }

    /// Returns the chunk at chunk coordinates, or `EMPTY` if absent.
    public static Chunk at(ChunkIndex chunks, int cx, int cz) {
        Object c = chunks.get(cx, cz);
        return c == null ? EMPTY : (Chunk) c;
    }

    /// Returns the block state at `x`, `y`, `z` in `chunks`, air in
    /// an absent chunk or section.
    public static int blockAt(ChunkIndex chunks, int x, int y, int z) {
        return at(chunks, x >> 4, z >> 4).block(x, y, z);
    }

    /// Returns the section that holds `x`, `y`, `z` in `chunks`, or null.
    public static Section sectionAt(ChunkIndex chunks, int x, int y, int z) {
        return at(chunks, x >> 4, z >> 4).section(sectionIndex(y));
    }

    /// Returns the lowest present section above section index `si`.
    public Section firstAbove(int si) {
        for (int i = Math.max(0, si + 1); i < COUNT; i++) {
            if (sections[i] != null) return sections[i];
        }
        return null;
    }

    /// Returns a starting section for `si` that inherits sky light from
    /// the first present section above it, or `Section.EMPTY`.
    public Section fresh(int si) {
        Section s = firstAbove(si);
        return s == null ? Section.EMPTY : s.below();
    }

    /// Returns a bit mask with bit `i` set when section `i` is present.
    public int present() {
        int mask = 0;
        for (int i = 0; i < COUNT; i++) {
            if (sections[i] != null) mask |= 1 << i;
        }
        return mask;
    }

    @Override
    public boolean equals(Object o) {
        if (o == this) return true;
        if (!(o instanceof Chunk)) return false;
        Section[] b = ((Chunk) o).sections;
        for (int i = 0; i < COUNT; i++) {
            if (!Objects.equals(sections[i], b[i])) return false;
        }
        return true;
    }

    @Override
    public int hashCode() {
        int h = 1;
        for (Section s : sections) {
            h = 31 * h + Objects.hashCode(s);
        }
        return h;
    }

    /// Writes the column to `out` in the snapshot form.
    public void save(DataOutput out) throws IOException {
        out.writeInt(present());
        for (Section s : sections) {
            if (s != null) s.save(out);
        }
    }

    /// Reads a column that `save` wrote.
    public static Chunk load(DataInput in) throws IOException {
        int mask = in.readInt();
        if ((mask >>> COUNT) != 0) {
            throw new IOException("chunk sections " + mask);
        }
        Section[] a = new Section[COUNT];
        for (int i = 0; i < COUNT; i++) {
            if ((mask & (1 << i)) != 0) a[i] = Section.load(in);
        }
        return new Chunk(a);
    }
}
