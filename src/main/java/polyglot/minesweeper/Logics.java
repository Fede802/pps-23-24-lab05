package polyglot.minesweeper;
import polyglot.Pair;
public interface Logics {

    ClickResult clickCell(Pair<Integer, Integer> cellPosition);

    void toggleFlag(Pair<Integer, Integer> cellPosition);

    GameCellData getCellStatus(Pair<Integer, Integer> cellPosition);

}
