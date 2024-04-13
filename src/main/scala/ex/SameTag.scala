package ex
import util.Sequences.Sequence

object SameTag:
  def unapply(s: Sequence[Item]): Option[Sequence[String]] =
    s.map(_.tags).foldLeft(s.head.orElse(Item.empty).tags)(
      (acc, seq) => acc.intersect(seq)) match
      case Sequence.Nil() => None
      case s: Sequence[String] => Some(s)

@main def testSameTag(): Unit =
  val item1 = Item(0,"item1", "tag1", "tag2")
  val item2 = Item(0,"item2", "tag1", "tag2", "tag3")
  val item3 = Item(0,"item3", "tag1", "tag2", "tag3", "tag4")
  val item4 = Item(0,"item1", "tag3", "tag4")

  def evaluateTags(s: Sequence[Item]): Unit =
    s match
      case SameTag(t) => println(s"common tags: ${t}")
      case _ => println("no tag in common")

  evaluateTags(Sequence.Nil())
  evaluateTags(Sequence(item1,item4))
  evaluateTags(Sequence(item1,item2,item3))
