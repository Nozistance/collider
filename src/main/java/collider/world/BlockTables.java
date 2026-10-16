package collider.world;

/// The tables of the block states, each indexed by state id.
///
/// @param types The block type of each state, or null.
/// @param shapes The shape kind of each state, or null.
/// @param solid True for a state that stops a body.
/// @param fullCube True for a state whose collision is a full cube.
/// @param useShape True for a state that occludes light by shape.
/// @param dampening The light that each state takes, 0 to 15.
/// @param emission The light that each state gives, 0 to 15.
/// @param touch The faces, one bit each, that the shape touches.
/// @param faces The six occlusion faces of each state, as four
///        longs of 16 by 16 bits, or null.
/// @param flags The bits of each state. 1 blocks motion, 2 catches
///        fire from lava, 4 ticks at random, 8 renders solid, 16 has a
///        full top face, 32 gives a signal, 64 is solid by old rules,
///        128 suffocates.
/// @param sturdy The faces, one bit each, that hold things.
public record BlockTables(
        Object[] types,
        Object[] names,
        Object[] shapes,
        boolean[] needsSupport,
        boolean[] attached,
        boolean[] liquid,
        boolean[] waterlogged,
        boolean[] falls,
        boolean[] canBeReplaced,
        boolean[] solid,
        boolean[] legacySolid,
        boolean[] fullCube,
        boolean[] blocksMotion,
        boolean[] useShape,
        boolean[] canOcclude,
        int[] dampening,
        int[] emission,
        int[] touch,
        Object[] faces,
        byte[] flags,
        byte[] sturdy,
        byte[] sturdyRigid,
        byte[] sturdyCenter) {
    private static BlockTables current;

    /// Returns the tables that install set last, or null before the
    /// first.
    public static BlockTables current() {
        return current;
    }

    /// Makes t the tables that current returns, and returns it.
    public static BlockTables install(BlockTables t) {
        current = t;
        return t;
    }
}
