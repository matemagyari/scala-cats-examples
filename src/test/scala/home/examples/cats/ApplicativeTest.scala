package home.examples.cats

import cats.Applicative
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

/**
  * Applicative extends Functor with an ap and pure method.
  *
  * If we view Functor as the ability to work with a single effect,
  * Applicative encodes working with multiple independent effects
  */
class ApplicativeTest extends FlatSpec with Matchers {

  "Creating an Applicative" should "work" in {

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
    }

    trait Applicative[F[_]] extends Functor[F] {
      def ap[A, B](ff: F[A ⇒ B])(fa: F[A]): F[B]
      def pure[A](a: A): F[A]

      def map[A, B](fa: F[A])(f: A ⇒ B): F[B] =
        ap(pure(f))(fa)
    }

    val optionApplicative = new Applicative[Option] {

      override def ap[A, B](ff: Option[A ⇒ B])(fa: Option[A]): Option[B] =
        ff.flatMap(f ⇒ fa.map(f))

      override def pure[A](a: A): Option[A] = Some(a)
    }

    optionApplicative.map(Option(3))(x ⇒ x + 1) shouldBe Option(4)
  }

  "Built-in Applicative" should "have mapN" in {

    import cats.implicits._

    val a: Option[Int] = Some(3)
    val b: Option[Int] = Some(2)
    Applicative[Option].map2(a, b)(_ + _) shouldBe Some(5)
  }

  "Built-in Applicative" should "have pure" in {

    import cats.implicits._

    Applicative[Option].pure(5) shouldBe Some(5)
  }

  "Built-in applicative syntax" should "work" in {

    import cats.implicits._

    val a: Option[Int] = Some(3)
    val b: Option[Int] = Some(2)

    (a, b).mapN(_ + _) shouldBe Some(5)
  }

  "Applicatives" should "compose" in {

    import cats.implicits._

    (Applicative[List] compose Applicative[Option]).pure(1) should be(List(Some(1)))
  }

  "Cartesian syntax" should "work" in {

    import cats.syntax.semigroup._

    {
      import cats.instances.string._
      "Hi " |+| "Joe" shouldBe "Hi Joe"
    }

    {
      import cats.instances.int._
      import cats.instances.option._
      Option(1) |+| Option(2) shouldBe Some(3)
    }

    //Applicative[Future].pure(1) |+| Applicative[Future].pure(2)

  }

}
