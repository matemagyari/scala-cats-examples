package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

//cats.Semigroupal is a type class that allows us to combine contexts
class SemigroupalTest extends FlatSpec with Matchers {

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

}
