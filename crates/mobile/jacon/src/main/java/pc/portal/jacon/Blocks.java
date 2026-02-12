package pc.portal.jacon;

import java.util.function.Function;
import java.util.function.Supplier;

// import java.util.Function;
import io.jbock.util.Either;

public class Blocks {
    private Blocks() {
    }

    public static <T> T blocks(int id, Supplier<Either<T,Integer>> []go) {
        while (true) {
            var g = go[id].get();
            if (g.isLeft()) {
                return g.getLeft().get();
            }
            id = g.getRight().get();
        }
        // return null;
    }
}
