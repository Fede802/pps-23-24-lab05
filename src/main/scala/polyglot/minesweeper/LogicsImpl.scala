package polyglot.minesweeper

import polyglot.{OptionToOptional, Pair}
import util.Optionals.Optional as myOptional
import util.Sequences.Sequence
import java.util.Optional
import scala.util.Random

case class GameCell(position: Pair[Integer, Integer], selected: Boolean = false, flagged: Boolean = false, mine: Boolean = false, minesAround: Int = 0):
  def select(): GameCell = this.copy(selected = true)
  def toggleFlag(): GameCell = this.copy(flagged = !flagged)
  def updateMinesAround(m: Int): GameCell = this.copy(minesAround = m)

class LogicsImpl(private val size: Int, private val mineToPlace: Int) extends Logics:
  private var grid: Sequence[GameCell] = makeGrid(size, mineToPlace)

  private def neighbours(gc1: GameCell): Sequence[GameCell] =
    def isNeighbours(c1: Pair[Integer, Integer], c2: Pair[Integer, Integer]) =
      Math.abs(c1.getX - c2.getX) <= 1 && Math.abs(c1.getY - c2.getY) <= 1 && !(c1 == c2)
    grid.filter(gc2 => isNeighbours(gc1.position, gc2.position))

  private def makeGrid(size: Int, mineToPlace: Int): Sequence[GameCell] =
    val (m, e) = Sequence(0 until size).combine(Sequence(0 until size)).map(p => Pair[Integer, Integer](p._1, p._2)).shuffle().splitAt(mineToPlace)
    grid = m.map(GameCell(_, mine = true)).concat(e.map(GameCell(_)))
    grid.map(gc => gc.updateMinesAround(neighbours(gc).filter(_.mine).length))

  def clickCell(cellPosition: Pair[Integer, Integer]): ClickResult =
    grid.find(gc => !gc.flagged && gc.position == cellPosition && !gc.selected) match
      case myOptional.Just(gc) =>
        grid = grid.map(gc => if !gc.flagged && gc.position == cellPosition then gc.select() else gc)
        if !gc.mine && gc.minesAround == 0 then neighbours(gc).foreach(gc => clickCell(gc.position))
        if !grid.filter(gc => gc.mine && gc.selected).isEmpty then ClickResult.LOSE else if grid.filter(gc => !gc.mine && !gc.selected).isEmpty then ClickResult.WIN else ClickResult.EMPTY
      case _ => ClickResult.EMPTY

  def toggleFlag(cellPosition: Pair[Integer, Integer]): Unit = grid = grid.map(gc => if gc.position == cellPosition then gc.toggleFlag() else gc)

  def getCellStatus(cellPosition: Pair[Integer, Integer]): Optional[GameCell] = OptionToOptional(grid.find(_.position == cellPosition))
