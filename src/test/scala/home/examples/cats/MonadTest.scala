package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class MonadTest extends FlatSpec with Matchers {

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

  "Custom Option Monad" should "look like this" in {
    import cats.Monad

    val optionMonad = new Monad[Option] {

      override def pure[A](x: A) = Some(x)

      override def flatMap[A, B](fa: Option[A])(f: A ⇒ Option[B]): Option[B] = fa.flatMap(f)

      @tailrec
      override def tailRecM[A, B](a: A)(f: A ⇒ Option[Either[A, B]]): Option[B] =
        f(a) match {
          case None ⇒ None
          case Some(Left(la)) ⇒ tailRecM(la)(f)
          case Some(Right(ra)) ⇒ Some(ra)
        }
    }
  }

  "Custom Monad for Tree" should "look like this" in {
    import cats.Monad

    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    implicit val treeMonad = new Monad[Tree] {
      override def pure[A](x: A) = Leaf(x)

      override def flatMap[A, B](fa: Tree[A])(f: A ⇒ Tree[B]): Tree[B] =
        fa match {
          case Leaf(x) ⇒ f(x)
          case Branch(left, right) ⇒
            Branch(flatMap(left)(f), flatMap(right)(f))
        }

      override def tailRecM[A, B](a: A)(f: A ⇒ Tree[Either[A, B]]): Tree[B] = ???
    }

    object Tree {
      def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
        Branch(left, right)
      def leaf[A](value: A): Tree[A] =
        Leaf(value)
    }
    import Tree._

    //import cats.syntax.functor._ //needed for tree.map

    val tree: Tree[Int] = branch(leaf(1), leaf(2))

    def func(t: Int): Tree[String] = branch(leaf(t.toString), leaf(t.toString))

    val expectedResult = branch(branch(leaf("1"), leaf("1")), branch(leaf("2"), leaf("2")))

    {
      treeMonad.flatMap(tree)(func) shouldBe expectedResult
    }

    {
      //map comes for free
      import cats.syntax.functor._ // for map
      tree.map(x ⇒ x.toString) shouldBe branch(leaf("1"), leaf("2"))
    }

    {
      import cats.syntax.flatMap._ // for flatMap
      tree.flatMap(func) shouldBe expectedResult
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

  "Monad" should "have ifM" in {

    import cats._
    import cats.implicits._

    Monad[Option].ifM(Option(true))(Option("truthy"), Option("falsy")) should be(
      Some("truthy")
    )

    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) should be(
      List(1, 2, 3, 4, 1, 2))

  }

}
