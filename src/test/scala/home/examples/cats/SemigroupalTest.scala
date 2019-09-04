package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

//cats.Semigroupal is a type class that allows us to combine contexts

/**
  *
  * cats.Semigroupal is a type class that allows us to combine contexts
  * Combination must be associative.
  * A semigroup is just the combine part of a monoid.
  * A Semigroup represents an addition or combination operation;
  * Monoid extends a Semigroup by adding an identity or “zero” element.
  */
class SemigroupalTest extends FlatSpec with Matchers {

  "Custom Semigroup" should "look like this" in {

    trait Semigroup[A] {
      def combine(a: A, b: A): A
    }

    val intPlusSemigroup = new Semigroup[Int] {
      override def combine(a: Int, b: Int) = a + b
    }

    intPlusSemigroup.combine(3, 4) shouldBe 7

  }
  "Custom Semigroupal" should "look like this" in {

    trait Semigroupal[F[_]] {
      def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
    }

    val optionSemigroupal = new Semigroupal[Option] {
      override def product[A, B](fa: Option[A], fb: Option[B]) =
        for {
          a ← fa
          b ← fb
        } yield (a, b)
    }

    optionSemigroupal.product(Some(1), Some("a")) shouldBe Some(1, "a")

    {
      import cats.Semigroupal
      import cats.instances.option._ // for Semigroupal

      Semigroupal[Option].product(Some(1), Some("a")) shouldBe Some(1, "a")
    }

    {
      import cats.instances.option._ // for Semigroupal
      import cats.syntax.apply._ // for tupled and mapN

      (Option(1), Option("a")).tupled shouldBe Some(1, "a")
    }

  }

  "Using mapN" should "look like this" in {

    import cats.syntax.apply._ // for tupled and mapN
    import cats.instances.option._ // for Semigroupal

    case class Cat(name: String, born: Int, color: String)

    val cat = Cat("Garfield", 1978, "Orange & black")

    (
      Option("Garfield"),
      Option(1978),
      Option("Orange & black")
    ).mapN(Cat.apply) shouldBe Some(cat)
  }

  "Semigroup" should "be commutative" in {

    import cats.implicits._

    val one: Option[Int] = Option(1)
    val two: Option[Int] = Option(2)
    val n: Option[Int] = None

    one |+| two shouldBe Option(3)
    n |+| n shouldBe n
    n |+| two shouldBe two
    two |+| n shouldBe two
  }

  "Semigroup" should "work with functions" in {
    import cats.implicits._
    import cats.kernel.Semigroup

    val fun: Int ⇒ Int = Semigroup[Int => Int].combine(_ + 1, _ * 10)
    fun.apply(6) shouldBe 67
  }

  it should "work with maps" in {
    import cats.implicits._
    import cats.kernel.Semigroup

    val aMap = Map("foo" → Map("bar" → 5))
    val anotherMap = Map("foo" → Map("bar" → 6))
    val combinedMap = Semigroup[Map[String, Map[String, Int]]].combine(aMap, anotherMap)

    combinedMap.get("foo") shouldBe Some(Map("bar" → 11))
  }

}
