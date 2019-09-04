package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

class FunctorTest extends FlatSpec with Matchers {

  "Basic examples" should "work" in {
    import cats.Functor
    import cats.implicits._

    Functor[Option].map(Option("Hello"))(_.length) shouldBe Option(5)
    Functor[Option].map(None: Option[String])(_.length) shouldBe None
  }

  "Functors" should "compose" in {
    import cats.Functor
    import cats.implicits._
    val listOpt = Functor[List] compose Functor[Option]
    val list = List(Some(1), None, Some(3))
    listOpt.map(list)(_ + 1) shouldBe List(Some(2), None, Some(4))
  }

  "Lift" should "work" in {
    import cats.Functor
    import cats.implicits._

    val lenOption: Option[String] ⇒ Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) shouldBe Some(5)
  }

  "Fproduct" should "work" in {
    import cats.Functor
    import cats.implicits._

    val source = List("Cats", "is", "awesome")
    val product: Map[String, Int] = {
      val tuples: Seq[(String, Int)] = Functor[List].fproduct(source)(_.length)
      tuples.toMap
    }

    product.get("Cats").getOrElse(0) shouldBe 4
    product.get("is").getOrElse(0) shouldBe 2
    product.get("awesome").getOrElse(0) shouldBe 7
  }

  "Creating a functor" should "work" in {

    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(f: A ⇒ B): F[B]
    }

    val SeqFunctor = new Functor[Seq] {
      override def map[A, B](fa: Seq[A])(f: A ⇒ B): Seq[B] =
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
