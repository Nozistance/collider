package collider.world.space;

/// The mob of one path search, where it stands and how it walks.
///
/// @param upStep How high it steps up without a jump.
/// @param maxFall How far it may fall.
/// @param floats True when it floats in water.
/// @param openDoors True when it opens doors.
/// @param passDoors True when it walks through doors.
/// @param overFences True when it walks over fences.
/// @param ctx The collision flags of its body.
public record PathMob(
        double x,
        double y,
        double z,
        double width,
        double height,
        double upStep,
        long maxFall,
        boolean floats,
        boolean openDoors,
        boolean passDoors,
        boolean overFences,
        int ctx
) {}
