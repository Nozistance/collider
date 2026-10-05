package collider.world.space;

import clojure.lang.Atom;

/// The sections that an explosion may reach, with the chunk columns
/// they come from.
/// In Java because Rays and Exposure, Java classes, read it.
///
/// @param grid The sections by grid x, z and y, null where a column
///     or a section is absent.
/// @param cols The chunk columns by grid x and z, null where the
///     column is absent.
/// @param cx0 The chunk x of the first cell.
/// @param cz0 The chunk z of the first cell.
/// @param sy0 The section y of the first cell.
/// @param ncx The size of the grid along x, in chunks.
/// @param ncz The size of the grid along z, in chunks.
/// @param nsy The size of the grid along y, in sections.
/// @param readAbsent The function that reads an absent chunk, or nil.
/// @param loaded The chunks that `readAbsent` gave, by id.
public record SectionGrid(
        Object[] grid,
        Object[] cols,
        int cx0,
        int cz0,
        int sy0,
        int ncx,
        int ncz,
        int nsy,
        Object readAbsent,
        Atom loaded) {}
