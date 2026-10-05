package collider.world;

/// The tables of the block states, each indexed by state id.
/// In Java because the Java classes read the tables from its fields.
///
/// @param types The block type of each state, or null.
/// @param names The block of each state.
/// @param shapes The shape kind of each state, or null.
/// @param needsSupport True for a state that breaks without support.
/// @param attached True for a state that hangs on a neighbour.
/// @param replaceable True for a state that a placement replaces.
/// @param liquid True for a liquid.
/// @param waterlogged True for a state that holds water.
/// @param falls True for a state that falls without a block under it.
/// @param canBeReplaced True for a state in the replaceable tag.
/// @param solid True for a state that stops a body.
/// @param legacySolid True for a state solid by the legacy rule.
/// @param fullCube True for a state whose collision is a full cube.
/// @param blocksMotion True for a state that blocks motion.
/// @param useShape True for a state that occludes light by shape.
/// @param canOcclude True for a state that can occlude neighbours.
/// @param dampening The light that each state takes, 0 to 15.
/// @param emission The light that each state gives, 0 to 15.
/// @param touch The faces, one bit each, that the shape touches.
/// @param faces The six occlusion faces of each state, as four
///        longs of 16 by 16 bits, or null.
/// @param resist The blast resistance of each state.
/// @param flags The flag bits of each state.
/// @param sturdy The faces, one bit each, that hold things.
/// @param sturdyRigid The faces that hold things rigidly.
/// @param sturdyCenter The faces that hold things at the center.
public record BlockTables(
        Object[] types,
        Object[] names,
        Object[] shapes,
        boolean[] needsSupport,
        boolean[] attached,
        boolean[] replaceable,
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
        double[] resist,
        byte[] flags,
        byte[] sturdy,
        byte[] sturdyRigid,
        byte[] sturdyCenter) {
    private static BlockTables current;

    /// Returns the tables that `install` set last, or null before
    /// the first.
    public static BlockTables current() {
        return current;
    }

    /// Makes `t` the tables that `current` returns, and returns it.
    public static BlockTables install(BlockTables t) {
        current = t;
        return t;
    }
}
