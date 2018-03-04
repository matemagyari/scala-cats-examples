package home.examples.cats

import scala.collection.immutable.Seq
import cats.data.State

object StateExamples2 extends App {

  type Stack[T] = Seq[T]

  def push[T](t: T, stack: Stack[T]): Stack[T] = t +: stack

  def pop[T](stack: Stack[T]): (Stack[T], Option[T]) =
    (stack.drop(1), stack.headOption)

  def peek[T](stack: Stack[T]): Option[T] = stack.headOption

  val s0: Stack[Int] = Seq(3, 2, 1)
  val s1 = push(4, s0)
  val s2 = push(5, s1)
  val e = peek(s2)
  val (s3, e1) = pop(s2)
  val (s4, e2) = pop(s3)
  val (s5, e3) = pop(s4)

  assert(e3 == Some(3))

//  val pop2 = State(pop)
  def pop2[T]() = State[Stack[T], Option[T]] {
    case xs => pop(xs)
  }
  def push2[T](a: T) = State[Stack[T], Unit] {
    case xs => (push(a, xs), ())
  }
  def peek2[T]() = State[Stack[T], Option[T]] {
    case xs => (xs, peek(xs))
  }

  val stateManip: State[Stack[Int], Option[Int]] = for {
    _ ← push2(3)
    _ ← push2(4)
    _ ← pop2[Int]
    _ ← pop2[Int]
    e ← peek2()
    x ← pop2[Int]
  } yield x

  val end = stateManip.runA(Seq(1, 2)).value

  assert(end == Some(1))
}

object StateExamples extends App {

  //direction: 0=North, 1=East, 2=South, 3=West

  final case class Coordinates(x: Int, y: Int)
  final case class Position(position: Coordinates, direction: Int)

  def turnRight(s: Position): (Position, String) = {
    val s2 = s.copy(direction = (s.direction + 1) % 4)
    (s2, "Turned right")
  }

  def turnLeft(s: Position): (Position, String) = {
    val s2 = s.copy(direction = (s.direction - 1) % 4)
    (s2, "Turned left")
  }

  def ahead(s: Position): (Position, String) = {
    val p2 = s.direction match {
      case 0 ⇒ s.position.copy(y = s.position.y + 1)
      case 1 ⇒ s.position.copy(x = s.position.x + 1)
      case 2 ⇒ s.position.copy(y = s.position.y - 1)
      case 3 ⇒ s.position.copy(x = s.position.x - 1)
    }
    (s.copy(position = p2), "Go ahead")
  }

  val startPosition = Position(Coordinates(0, 0), direction = 0)

  val (p1, a1) = ahead(startPosition)
  val (p2, a2) = turnRight(p1)
  val (p3, a3) = ahead(p2)
  val (p4, a4) = ahead(p3)

  println(s"$p4 [$a1|$a2|$a3|$a4]")

  val turnRight2: State[Position, String] = State(turnRight)
  val turnLeft2: State[Position, String] = State(turnLeft)
  val ahead2: State[Position, String] = State(ahead)

  val goAround: State[Position, String] =
    for {
      _ <- ahead2
      _ <- turnRight2
      _ <- ahead2
      x <- ahead2
    } yield x

  goAround.runA(startPosition).value

//  import cats.data.Writer
//  def x(s: Position): (Position, List[String]) = ???
//  val turnRight3: Writer[Position, List[String]] = Writer(x)
//  val turnLeft3: Writer[Position, String] = Writer(turnLeft)
//  val ahead3: Writer[Position, String] = Writer(ahead)

}
