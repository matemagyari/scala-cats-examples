package home.examples.cats

import cats.Monad
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

class MonadExamplesTest extends FlatSpec with Matchers {

  "Creating a monad" should "work" in {

    trait Monad[F[_]] {
      def pure[A](a: A): F[A]
      def flatMap[A, B](fa: F[A])(f: A ⇒ F[B]): F[B]

      //from pure and flatMap
      def map[A, B](a: F[A])(f: A ⇒ B): F[B] = {
        val f2: A ⇒ F[B] = f andThen pure
        flatMap(a)(f2)
      }
    }
  }

  "Built-in monads" should "work" in {
    import cats.Monad
    {
      import cats.instances.option._
      Monad[Option].pure(1) shouldBe Some(1)
    }
    {
      import cats.instances.list._
      Monad[List].pure(1) shouldBe List(1)
      Monad[List].flatMap(List(1, 2))(x ⇒ List(x, x)) shouldBe List(1, 1, 2, 2)
    }
  }

  "Built-in monad syntax" should "work" in {
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad

    {
      import cats.syntax.applicative._ // for pure

      1.pure[Option] shouldBe Some(1)
      1.pure[List] shouldBe List(1)
    }

    {
      import cats.Monad
      import cats.syntax.functor._ // for map
      import cats.syntax.flatMap._ // for flatMap

      def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
        for {
          av ← a
          bv ← b
        } yield av * av + bv * bv

      sumSquare(List(1, 2), List(3, 4)) shouldBe List(10, 17, 13, 20)

      sumSquare(Option(3), Option(4)) shouldBe Some(25)

      {
        import cats.Id // for Monad
        sumSquare(3: Id[Int], 4: Id[Int]) shouldBe 25
      }
    }

  }

  "Pure/map/flatMap implementation for Id" should "work" in {

    class IdMonad[A] {
      def pure[A](a: A): A = a

      //they are identical!
      def flatMap[A, B](a: A)(f: A ⇒ B): B = f(a)
      def map[A, B](a: A)(f: A ⇒ B): B = f(a)
    }

  }

}
