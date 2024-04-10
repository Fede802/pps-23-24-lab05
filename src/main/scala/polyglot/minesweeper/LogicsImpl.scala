package polyglot.minesweeper

import polyglot.Pair

trait GameCell:
  def cellPosition: Pair[Int,Int]
  def selected: Boolean
  def flagged: Boolean
  def select(): Unit
  def toggleFlag(): Unit
  def entityType: EntityType

object GameCell:
  private def apply(x: Int, y:Int, entityType2: EntityType): GameCell = GameCellImpl(Pair[Int,Int](x,y),entityType2)
  def mine(position: Position): GameCell = GameCellImpl(position, EntityType.MINE)
  def empty(position: Position): GameCell = GameCellImpl(position, EntityType.EMPTY)
  private class GameCellImpl(val cellPosition: Pair[Int,Int], val entityType: EntityType, private var _selected: Boolean = false, private var _flagged: Boolean = false) extends GameCell:
    def selected: Boolean = _selected
    def flagged: Boolean = _flagged
    def select(): Unit = {assert(!_selected); _selected = true}
    def toggleFlag(): Unit = _flagged = !_flagged


case class GameCellData()

class LogicsImpl(private val size: Int, private val mineToPlace: Int) extends Logics:

  override def clickCell(cellPosition: Pair[Integer, Integer]): ClickResult = ClickResult.WIN

  override def toggleFlag(cellPosition: Pair[Integer, Integer]): Unit = ()

  override def getCellStatus(cellPosition: Pair[Integer, Integer]): GameCellData = GameCellData()

