package home.examples.cats

import cats.Applicative

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

class TraverseTest extends FlatSpec with Matchers {

  "Custom Future traverse implementation" should "look like this" in {

    def traverse[A, B](as: Seq[A])(func: A ⇒ Future[B]): Future[Seq[B]] =
      as.foldLeft(Future(Seq.empty[B])) { (acc, element) ⇒
        for {
          bs ← acc
          futureElement ← func(element)
        } yield bs :+ futureElement
      }

    val result: Future[Seq[Int]] = traverse(Seq(1, 2, 3))(x ⇒ Future(x))

    await(result) shouldBe Seq(1, 2, 3)
  }

  "listTraverse and listSequence" should "work" in {

    import scala.language.higherKinds
    import cats.syntax.applicative._ // for pure
    import cats.syntax.apply._ // for mapN

    def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A ⇒ F[B]): F[List[B]] =
      list.foldLeft(List.empty[B].pure[F]) { (acc, element) ⇒
        (acc, func(element)).mapN(_ :+ _)
      }

    def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
      listTraverse(list)(identity)

    {
      //examples for F as Future

      import cats.instances.future._ // for Applicative

      def doAysnchTransformation(n: Int): Future[Int] = Future(n * 2)
      val result: Future[Seq[Int]] = listTraverse(List(1, 2, 3))(doAysnchTransformation)
      await(result) shouldBe Seq(2, 4, 6)

      val result2: Future[Seq[Int]] = listSequence(List(Future(1), Future(2)))
      await(result2) shouldBe Seq(1, 2)
    }

    {
      //examples for F as List

      import cats.instances.list._ // for Applicative

      def makeList(n: Int): List[Int] = List(n * 2)
      val result: List[Seq[Int]] = listTraverse(List(1, 2, 3))(makeList)
      result shouldBe List(List(2, 4, 6))

      val result2: List[Seq[Int]] = listSequence(List(List(1, 2), List(3, 4)))
      result2 shouldBe List(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
    }

    {
      //examples for F as Validated

      import cats.data.Validated
      import cats.data.Validated.Invalid
      import cats.instances.list._ // for Monoid

      type ErrorsOr[A] = Validated[List[String], A]

      def evaluate(n: Int): ErrorsOr[Int] =
        if (n % 2 == 0)
          Validated.valid(n / 2)
        else
          Validated.invalid(List(s"$n is not even"))

      def process(inputs: List[Int]): ErrorsOr[Seq[Int]] =
        listTraverse(inputs)(evaluate)

      val result: ErrorsOr[Seq[Int]] = process(List(1, 2, 3, 4))

      result shouldBe Invalid(List("1 is not even", "3 is not even"))

    }
  }

  private def await[A](f: Future[A]): A = Await.result(f, 2 seconds)
}
