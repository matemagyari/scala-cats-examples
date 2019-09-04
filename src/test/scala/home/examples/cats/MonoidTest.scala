package home.examples.cats

import cats.Monoid
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.immutable.Seq

class MonoidTest extends FlatSpec with Matchers {

  "Basic examples" should "work" in {
    import cats.implicits._

    {
      Monoid[String].empty shouldBe ""
      Monoid[String].combineAll(List("a", "b", "c")) shouldBe "abc"
      Monoid[String].combineAll(List()) shouldBe ""
    }

    {
      Monoid[Map[String, Int]]
        .combineAll(List(Map("a" → 1, "b" → 2), Map("a" → 3))) shouldBe Map("a" → 4, "b" → 2)

      Monoid[Map[String, Int]].combineAll(List()) shouldBe Map()
    }

    {
      val l = List(1, 2, 3, 4, 5)
      l.foldMap(identity) shouldBe 15
      l.foldMap(i ⇒ i.toString) shouldBe "12345"
    }

    {
      implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
        new Monoid[(A, B)] {
          def combine(x: (A, B), y: (A, B)): (A, B) = {
            val (xa, xb) = x
            val (ya, yb) = y
            (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
          }
          def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
        }

      val l = List(1, 2, 3, 4, 5)
      //l.foldMap(i ⇒ (i, i.toString)) shouldBe (15, "12345")
    }
  }

  "Simple monoid" should "work" in {

    val booleanMonoid = new Monoid[Boolean] {
      override def empty: Boolean = true
      override def combine(x: Boolean, y: Boolean): Boolean = x && y
    }

    val intMonoid = new Monoid[Int] {
      override def empty: Int = 0
      override def combine(x: Int, y: Int): Int = x + y
    }

    intMonoid.combine(1, 2) shouldBe 3
    intMonoid.combineAll(Seq(1, 2, 3)) shouldBe 6
    intMonoid.combineAllOption(Seq.empty) shouldBe None
  }

  "Combine monoids" should "work" in {

    implicit def setUnionMonoid[A] = new Monoid[Set[A]] {
      override def empty: Set[A] = Set.empty[A]
      override def combine(x: Set[A], y: Set[A]): Set[A] = x ++ y
    }

    val intSetUnionMonoid = Monoid[Set[Int]]

    intSetUnionMonoid.combine(Set(1, 2), Set(2, 3)) shouldBe Set(1, 2, 3)
  }

  "Built-in monoid" should "work" in {
    import cats.instances.int._

    Monoid[Int].combine(1, 2) shouldBe 3
  }

  "Monoid syntax" should "work" in {

    import cats.syntax.semigroup._
    {
      import cats.instances.string._

      val str = "Hi, " |+| "Joe" |+| Monoid[String].empty

      str shouldBe "Hi, Joe"
    }

    {
      import cats.instances.int._ // for Monoid
      import cats.instances.option._ // for Monoid

      Option(1) |+| Option(2) shouldBe Option(3)
    }

    {
      import cats.instances.int._
      import cats.instances.map._
      Map("a" → 1) |+| Map("b" → 2) shouldBe Map("a" → 1, "b" → 2)
    }
  }

  "Generic code for Monoids" should "work" in {
    import cats.syntax.semigroup._
    def addAll[A](as: Seq[A])(implicit m: Monoid[A]): A =
      as.foldLeft(m.empty)(_ |+| _)

    {
      import cats.instances.int._
      addAll(Seq(1, 2, 3)) shouldBe 6
    }

    {
      import cats.instances.string._
      addAll(Seq("a", "b")) shouldBe "ab"
    }

  }

}
