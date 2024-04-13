package polyglot.minesweeper;

import polyglot.Pair;
import java.util.Optional;
public interface Logics {

    ClickResult clickCell(Pair<Integer, Integer> cellPosition);

    void toggleFlag(Pair<Integer, Integer> cellPosition);

    Optional<GameCell> getCellStatus(Pair<Integer, Integer> cellPosition);

}