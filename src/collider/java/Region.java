package collider.java;

import clojure.lang.Atom;

/// The sections an explosion may reach, a grid `ncx` by `ncz` by
/// `nsy` whose first cell covers section `cx0`, `cz0`, `sy0`, and the
/// chunk columns they come from. A column absent from the grid is
/// asked of `readAbsent`; what it answers gathers in `loaded`.
public final class Region {

    public final Object[] grid;
    public final Object[] cols;
    public final int cx0, cz0, sy0;
    public final int ncx, ncz, nsy;
    public final Object readAbsent;
    public final Atom loaded;

    public Region(Object[] grid, Object[] cols, int cx0, int cz0,
                  int sy0, int ncx, int ncz, int nsy,
                  Object readAbsent, Atom loaded) {
        this.grid = grid;
        this.cols = cols;
        this.cx0 = cx0;
        this.cz0 = cz0;
        this.sy0 = sy0;
        this.ncx = ncx;
        this.ncz = ncz;
        this.nsy = nsy;
        this.readAbsent = readAbsent;
        this.loaded = loaded;
    }
}
