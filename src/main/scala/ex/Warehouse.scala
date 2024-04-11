package ex

import ex.Warehouse.{SameTagV1, SameTagV2, SameTagV3}
import util.Optionals.Optional
import util.Sequences.*

import scala.annotation.tailrec
trait Item:
  def code: Int
  def name: String
  def tags: Sequence[String]

object Item:
  def apply(code: Int, name: String, tags: String*): Item = ItemImpl(code,name,Sequence(tags*))
  def empty: Item = ItemImpl(-1,"",Sequence(""))
  private case class ItemImpl(code: Int, name: String, tags: Sequence[String]) extends Item
/**
 * A warehouse is a place where items are stored.
 */
trait Warehouse:
  /**
   * Stores an item in the warehouse.
   * @param item the item to store
   */
  def store(item: Item): Unit
  /**
   * Searches for items with the given tag.
   * @param tag the tag to search for
   * @return the list of items with the given tag
   */
  def searchItems(tag: String): Sequence[Item]
  /**
   * Retrieves an item from the warehouse.
   * @param code the code of the item to retrieve
   * @return the item with the given code, if present
   */
  def retrieve(code: Int): Optional[Item]
  /**
   * Removes an item from the warehouse.
   * @param item the item to remove
   */
  def remove(item: Item): Unit
  /**
   * Checks if the warehouse contains an item with the given code.
   * @param itemCode the code of the item to check
   * @return true if the warehouse contains an item with the given code, false otherwise
   */
  def contains(itemCode: Int): Boolean
  def items: Sequence[Item]
end Warehouse

object Warehouse:

  def apply(): Warehouse = WarehouseImpl()

  private case class WarehouseImpl() extends Warehouse:
    var items: Sequence[Item] = Sequence()

    def store(item: Item): Unit = items = items.concat(Sequence(item))

    def searchItems(tag: String): Sequence[Item] = items.filter(_.tags.contains(tag))

    def retrieve(code: Int): Optional[Item] = items.find(_.code == code)

    def remove(item: Item): Unit = items.filter(_ != item)

    def contains(itemCode: Int): Boolean = retrieve(itemCode).isEmpty

  object SameTagV1:
    @tailrec
    def unapply(s: Sequence[Sequence[String]]): Option[Sequence[String]]=
      s match
        case Sequence.Cons(firstItemTagList, Sequence.Cons(secondItemTagList, itemTail)) =>
          val intersection = firstItemTagList.intersect(secondItemTagList)
          if intersection.isEmpty then Option.empty else unapply(Sequence(intersection).concat(itemTail))
        case Sequence.Cons(firstItemTagList, Sequence.Nil()) => Option.apply(firstItemTagList)
        case Sequence.Nil() => Option.empty

  object SameTagV2:
    def unapply(s: Sequence[Item]): Option[Sequence[String]] =
      s.head match
        case Optional.Just(a) => s.map(_.tags).foldLeft(a.tags)(
          (acc, seq) => acc.intersect(seq)) match
          case Sequence.Nil() => Option.empty
          case s: Sequence[String] => Option.apply(s)
        case Optional.Empty() => Option.empty

  object SameTagV3:
    def unapply(s: Sequence[Item]): Option[Sequence[String]] =
      s.map(_.tags).foldLeft(s.head.orElse(Item.empty).tags)(
          (acc, seq) => acc.intersect(seq)) match
          case Sequence.Nil() => Option.empty
          case s: Sequence[String] => Option.apply(s)



@main def mainWarehouse(): Unit =
  val warehouse = Warehouse()
  val warehouse2 = Warehouse()
  val dellXps = Item(33, "Dell XPS 15","notebook")
  val dellInspiron = Item(34, "Dell Inspiron 13", "notebook")
  val xiaomiMoped = Item(35, "Xiaomi S1", "moped", "notebook","mobility")
  val xiaomiMoped2 = Item(35, "Xiaomi S1", "moped","mobility")
  warehouse2.store(dellXps)
  warehouse2.store(dellInspiron)
  warehouse2.store(xiaomiMoped2)
  println:
    warehouse.contains(dellXps.code) // false
  println:
    warehouse.store(dellXps) // side effect, add dell xps to the warehouse
  println:
    warehouse.contains(dellXps.code) // true
  println:
    warehouse.store(dellInspiron) // side effect, add dell Inspiron to the warehouse
  println:
    warehouse.store(xiaomiMoped) // side effect, add xiaomi moped to the warehouse
  println:
    warehouse.searchItems("mobility") // Sequence(xiaomiMoped)
  println:
    warehouse.searchItems("notebook") // Sequence(dellXps, dell Inspiron)
  println:
    warehouse.retrieve(11) // None
  println:
    warehouse.retrieve(dellXps.code) // Just(dellXps)
  println:
    warehouse.remove(dellXps) // side effect, remove dell xps from the warehouse
  println:
    warehouse.retrieve(dellXps.code) // None

  @tailrec
  def filterTags(s: Sequence[Sequence[String]]): Sequence[String] =
    s match
      case Sequence.Cons(firstItemTagList, Sequence.Cons(secondItemTagList, itemTail)) =>
          val intersection = firstItemTagList.intersect(secondItemTagList)
          if intersection.isEmpty then Sequence() else filterTags(Sequence(intersection).concat(itemTail))
      case Sequence.Cons(firstItemTagList, Sequence.Nil()) => firstItemTagList
      case Sequence.Nil() => Sequence()

  warehouse.items match
    case SameTagV3(t) => println(s"common tags: ${t}")
    case _ => println("no tag in common")

  warehouse2.items match
    case SameTagV3(t) => println(s"common tags: ${t}")
    case _ => println("no tag in common")

  println(filterTags(warehouse.items.map(_.tags)))



/** Hints:
 * - Implement the Item with a simple case class
 * - Implement the Warehouse keeping a private List of items
 * - Start implementing contains and store
 * - Implement searchItems using filter and contains
 * - Implement retrieve using find
 * - Implement remove using filter
 * - Refactor the code of Item accepting a variable number of tags (hint: use _*)
*/