package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

/**
  * Applicative extends Functor with an ap and pure method.
  *
  * If we view Functor as the ability to work with a single effect,
  * Applicative encodes working with multiple independent effects
  */
class ApplyTest extends FlatSpec with Matchers {

  "Creating an Apply" should "work" in {
    import cats._

    implicit val optionApply: Apply[Option] = new Apply[Option] {
      def ap[A, B](f: Option[A => B])(fa: Option[A]): Option[B] =
        fa.flatMap(a ⇒ f.map(ff ⇒ ff(a)))

      def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa map f

      override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] =
        fa.flatMap(a ⇒ fb.map(b ⇒ (a, b)))
    }

    implicit val listApply: Apply[List] = new Apply[List] {
      def ap[A, B](f: List[A => B])(fa: List[A]): List[B] =
        fa.flatMap(a ⇒ f.map(ff ⇒ ff(a)))

      def map[A, B](fa: List[A])(f: A => B): List[B] = fa map f

      override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] =
//        fa.zip(fb)
        fa.flatMap(a ⇒ fb.map(b ⇒ (a, b)))
    }
  }

  "Built-in Applies" should "have map" in {
    import cats.implicits._
    import cats.Apply

    val intToString: Int ⇒ String = _.toString
    val double: Int ⇒ Int = _ * 2
    val addTwo: Int ⇒ Int = _ + 2

    Apply[Option].map(Some(1))(intToString) should be(Some("1"))
    Apply[Option].map(Some(1))(double) should be(Some(2))
    Apply[Option].map(None)(addTwo) should be(None)
  }

  "Built-in Applies" should "compose" in {
    import cats.implicits._
    import cats.Apply

    val listOpt = Apply[List] compose Apply[Option]
    val plusOne = (x: Int) ⇒ x + 1
    listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3))) should be(
      List(Some(2), None, Some(4)))
  }

  "Built-in Applies" should "have apN and mapN" in {
    import cats.implicits._
    import cats.Apply

    val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c
    Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) should be(Some(6))

    Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) should be(Some(6))
  }

  "Built-in Applies" should "have tupleN" in {
    import cats.implicits._
    import cats.Apply

    Apply[Option].tuple2(Some(1), Some(2)) should be(
      Some(1, 2)
    )
  }

}
