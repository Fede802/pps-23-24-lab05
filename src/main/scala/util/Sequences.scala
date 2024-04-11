package util
import Optionals.Optional.*
import util.Optionals.Optional

import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists
  
  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

    override def toString: String = this match
      case Cons(h, t) => s"$h :: $t"
      case Nil() => "nil"

  object Sequence:
    def apply[A](elements: A*): Sequence[A] =
      var sequence: Sequence[A] = Nil()
      for e <- elements do
        sequence = Cons(e, sequence)
      sequence.reverse()

    def empty[A]: Sequence[A] = Nil()

    def count[A](s: Sequence[A]): Int =
      @tailrec
      def _count(s: Sequence[A], i: Int): Int = s match
        case Cons(_,t) => _count(t,i+1)
        case _ => i
      _count(s,0)

    extension [A](sequence: Sequence[A])
      def isEmpty: Boolean = sequence match
        case Sequence.Nil() => true
        case _ => false
      def intersect(sequence2: Sequence[A]): Sequence[A]=
        sequence.filter(i => sequence2.contains(i))

      def head: Optional[A] = sequence match
        case Cons(h, _) => Just(h)
        case _ => Empty()

      def foreach(action : A => Unit): Unit = sequence match
        case Cons(h,t) => {action(h); t.foreach(action)}
        case _ => ()

      def concat(other: Sequence[A]): Sequence[A] = sequence match
        case Cons(h, t) => Cons(h, t.concat(other))
        case _ => other

      def flatMap[B](f: A => Sequence[B]): Sequence[B] = sequence match
        case Cons(h, t) => f(h).concat(t.flatMap(f))
        case _ => Nil()

      def map[B](f: A => B): Sequence[B] = sequence.flatMap(x => Cons(f(x), Nil()))

      def filter(f: A => Boolean): Sequence[A] = sequence.flatMap:
        case x if f(x) => Cons(x, Nil())
        case _ => Nil()

      def find(f: A => Boolean): Optional[A] = sequence match
        case Cons(h, t) if f(h) => Just(h)
        case Cons(_, t) => t.find(f)
        case _ => Empty()

      def contains(e: A): Boolean = !sequence.find(_ == e).isEmpty

      def reverse(): Sequence[A] = sequence match
        case Cons(h, t) => t.reverse().concat(Cons(h, Nil()))
        case _ => Nil()

      @annotation.tailrec
      def foldLeft(a: Sequence[String])(f: (Sequence[String], A) => Sequence[String]): Sequence[String] = sequence match
        case Cons(h, t) => t.foldLeft(f(a, h))(f)
        case Nil() => a

@main def trySequences =
  import Sequences.* 
  val sequence = Sequence(1, 2, 3)
  println(sequence)
  println(sequence.head)
  println(sequence.map(_ * 2))
  println(sequence.flatMap(x => Sequence(x, x * 2)))
  println(sequence.filter(_ % 2 == 0))
  println(sequence.concat(Sequence(4, 5, 6)))
  println(sequence.find(_ % 2 == 0))
  println(sequence.contains(2))



