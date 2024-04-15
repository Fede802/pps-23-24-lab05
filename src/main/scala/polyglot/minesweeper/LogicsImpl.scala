package polyglot.minesweeper

import polyglot.OptionToOptional
import util.Optionals.Optional as myOptional
import util.Sequences.Sequence
import java.util.Optional

case class GameCell(p: Position, selected: Boolean = false, flagged: Boolean = false, mine: Boolean = false, minesAround: Int = -1):
  def select(): GameCell = this.copy(selected = true)
  def toggleFlag(): GameCell = this.copy(flagged = !flagged)
  def updateMinesAround(m: Int): GameCell = this.copy(minesAround = m)

class LogicsImpl(size: Int, mineToPlace: Int) extends Logics:
  private var grid: Sequence[GameCell] = makeGrid(size, mineToPlace)

  private def neighbours(gc1: GameCell): Sequence[GameCell] =
    def isNeighbours(c1: Position, c2: Position) =
      Math.abs(c1.x - c2.x) <= 1 && Math.abs(c1.y - c2.y) <= 1 && !(c1 == c2)
    grid.filter(gc2 => isNeighbours(gc1.p, gc2.p))

  private def makeGrid(size: Int, mineToPlace: Int): Sequence[GameCell] =
    val (m, e) = Sequence(0 until size).combine(Sequence(0 until size)).map(p => Position(p._1, p._2)).shuffle().splitAt(mineToPlace)
    grid = m.map(GameCell(_, mine = true)).concat(e.map(GameCell(_)))
    grid.map(gc => gc.updateMinesAround(neighbours(gc).filter(_.mine).length))

  def clickCell(p: Position): ClickResult =
    grid.find(gc => !gc.flagged && gc.p == p && !gc.selected) match
      case myOptional.Just(gc) =>
        grid = grid.map(gc => if gc.p == p then gc.select() else gc)
        if !gc.mine && gc.minesAround == 0 then neighbours(gc).foreach(gc => clickCell(gc.p))
        if !grid.filter(gc => gc.mine && gc.selected).isEmpty then ClickResult.LOSE
        else if grid.filter(gc => !gc.mine && !gc.selected).isEmpty then ClickResult.WIN
        else ClickResult.EMPTY
      case _ => ClickResult.EMPTY

  def toggleFlag(p: Position): Unit = grid = grid.map(gc => if gc.p == p then gc.toggleFlag() else gc)

  def getCellStatus(p: Position): Optional[GameCell] = OptionToOptional(grid.find(_.p == p))
