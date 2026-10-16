package collider.world.space;

/// The mob of one path search, where it stands and how it walks.
///
/// @param upStep How many blocks it steps up without a jump.
/// @param maxFall How many blocks it may fall.
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
