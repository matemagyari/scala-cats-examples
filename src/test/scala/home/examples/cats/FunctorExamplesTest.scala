package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

class FunctorExamplesTest extends FlatSpec with Matchers {

  "Creating a functor" should "work" in {

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
    }

    val SeqFunctor = new Functor[Seq] {
      override def map[A, B](fa: Seq[A])(f: A ⇒ B) =
        fa.map(f)
    }

    SeqFunctor.map(Seq(1, 2, 3))(x ⇒ x + 1) shouldBe Seq(2, 3, 4)
  }

  "Built-in functor" should "work" in {
//    import scala.language.higherKinds
    import cats.Functor

    {
      import cats.instances.list._ // for Functor
      Functor[List].map(List(1, 2))(x ⇒ x + 1) shouldBe List(2, 3)
    }

    {
      import cats.instances.option._ // for Functor
      Functor[Option].map(Option(1))(x ⇒ x + 1) shouldBe Option(2)
    }

  }

  "Functor" should "work on Tree" in {
    sealed trait Tree[+A]
    final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    import cats.Functor
    implicit val treeFunctor = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A ⇒ B): Tree[B] =
        fa match {
          case Branch(left, right) ⇒ Branch(map(left)(f), map(right)(f))
          case Leaf(a) ⇒ Leaf(f(a))
        }
    }

    def func(i: Int): String = i.toString

    {
      val tree: Tree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
      val result: Tree[String] = Functor[Tree].map(tree)(func)
      result shouldBe Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
    }

    {

      object Tree {
        def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
          Branch(left, right)
        def leaf[A](value: A): Tree[A] =
          Leaf(value)
      }
      import Tree._

      import cats.syntax.functor._ //needed for tree.map

      val tree: Tree[Int] = branch(branch(leaf(1), leaf(2)), leaf(3))
      tree.map(func) shouldBe branch(branch(leaf("1"), leaf("2")), leaf("3"))
    }
  }

}
