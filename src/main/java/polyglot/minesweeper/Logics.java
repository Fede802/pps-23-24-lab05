package polyglot.minesweeper;

import polyglot.Pair;
import java.util.Optional;
public interface Logics {

    ClickResult clickCell(Position cellPosition);

    void toggleFlag(Position cellPosition);

    Optional<GameCell> getCellStatus(Position cellPosition);

}