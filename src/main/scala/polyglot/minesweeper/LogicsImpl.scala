package polyglot.minesweeper

import polyglot.Pair
import util.Optionals.Optional
import util.Sequences.Sequence

import scala.util.Random




trait GameCell:
  def cellPosition: Pair[Integer, Integer]
  def selected: Boolean
  def flagged: Boolean
  def select(): Unit
  def toggleFlag(): Unit
  def entityType: EntityType

object GameCell:
  private def apply(x: Int, y:Int, entityType2: EntityType): GameCell = GameCellImpl(Pair[Integer, Integer](x,y),entityType2)
  def mine(x: Int, y:Int): GameCell = GameCell(x,y, EntityType.MINE)
  def empty(x: Int, y:Int): GameCell = GameCell(x,y, EntityType.EMPTY)
  private class GameCellImpl(val cellPosition: Pair[Integer, Integer], val entityType: EntityType, private var _selected: Boolean = false, private var _flagged: Boolean = false) extends GameCell:
    def selected: Boolean = _selected
    def flagged: Boolean = _flagged
    def select(): Unit = {assert(!_selected); _selected = true}
    def toggleFlag(): Unit = _flagged = !_flagged

case class GameCellData(clicked: Boolean, flagged: Boolean, mine: Boolean, minesAround: Int)

trait Grid:
  def size: Int
  def cellAt(x: Int, y: Int): GameCell
  def neighbours(x: Int, y: Int): Sequence[GameCell]

object Grid:
  private val rand: Random = Random()
  private def generatePosition(maxIndex: Int): Pair[Integer, Integer] = Pair(rand.nextInt(maxIndex), rand.nextInt(maxIndex))
  def apply(gridSize: Int, mineNumber: Int): Grid =
    require(mineNumber <= gridSize * gridSize)
    var minePositions = Sequence[Pair[Integer, Integer]]()
    while (Sequence.count(minePositions) != mineNumber)
      val np = generatePosition(gridSize)
      if minePositions.contains(np) then minePositions = minePositions.concat(Sequence(np))
    GridImpl(gridSize, minePositions)
  private class GridImpl(val size: Int, mines: Sequence[Pair[Integer, Integer]]) extends Grid:
    private var board: Sequence[GameCell] = Sequence()
      for
        i <- 0 until 10
        j <- 0 until 10
      do if mines.contains(Pair(i,j))
          then board = board.concat(Sequence(GameCell.mine(i,j)))
          else board = board.concat(Sequence(GameCell.empty(i,j)))
    def cellAt(x: Int, y: Int): GameCell = board.find(_.cellPosition == Pair(x,y)).get()
    private def isNeighbours(firstCell: Pair[Integer, Integer], secondCell: Pair[Integer, Integer]) =
      val distanceX = Math.abs(firstCell.getX - secondCell.getX)
      val distanceY = Math.abs(firstCell.getY - secondCell.getY)
      distanceX <= 1 && distanceY <= 1 && !(firstCell == secondCell)
    def neighbours(x: Int, y: Int): Sequence[GameCell] = board.filter(c => isNeighbours(c.cellPosition, Pair(x,y)))

trait Logics:
  def clickCell(cellPosition: Pair[Integer, Integer]): ClickResult
  def toggleFlag(cellPosition: Pair[Integer, Integer]): Unit
  def getCellStatus(cellPosition: Pair[Integer, Integer]): GameCellData

class LogicsImpl(private val size: Int, private val mineToPlace: Int) extends Logics:
  private val grid: Grid = Grid(size,mineToPlace)
  private var gameLost: Boolean = false

  private def handleCellClick(gameCell: GameCell): Optional[ClickResult] =
    var r = Optional.Empty[ClickResult]()
    if gameCell.entityType == EntityType.MINE
      then
        gameLost = true
        r = Optional.Just(ClickResult.LOSE)
    if numberOfMinesAround(gameCell.cellPosition) == 0
      then handleRecursiveAutoClick(gameCell)
    if allEmptyCellClicked then r = Optional.Just(ClickResult.WIN)
    r

  private def handleRecursiveAutoClick(gameCell: GameCell): Unit =
    grid.neighbours(gameCell.cellPosition.getX, gameCell.cellPosition.getY).foreach(c => clickCell(c.cellPosition))

  private def allEmptyCellClicked: Boolean = {
    var r = true
    for (i <- 0 until this.grid.size) {
      for (j <- 0 until this.grid.size) {
        val gameCell = this.grid.cellAt(i, j)
        if ((gameCell.entityType == EntityType.EMPTY) && !gameCell.selected) then r = false
      }
    }
    r
  }

  override def clickCell(cellPosition: Pair[Integer, Integer]): ClickResult =
    var r = ClickResult.EMPTY
    if (!gameLost)
    then
      val gc = grid.cellAt(cellPosition.getX, cellPosition.getY)
      if (!gc.selected && !gc.flagged)
      then
        gc.select()
        r = handleCellClick(gc).orElse(ClickResult.EMPTY)
    r

  override def toggleFlag(cellPosition: Pair[Integer, Integer]): Unit =
    val gc = grid.cellAt(cellPosition.getX,cellPosition.getY)
    if(!gameLost && !gc.selected)
      gc.toggleFlag()

  override def getCellStatus(cellPosition: Pair[Integer, Integer]): GameCellData =
    val gameCell = grid.cellAt(cellPosition.getX,cellPosition.getY)
    GameCellData(gameCell.selected,gameCell.flagged,gameCell.entityType == EntityType.MINE, numberOfMinesAround(cellPosition))

  private def numberOfMinesAround(cellPosition: Pair[Integer, Integer]): Int =
    Sequence.count(grid.neighbours(cellPosition.getX, cellPosition.getY).filter(_.entityType == EntityType.MINE))

