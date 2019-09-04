package home.examples.cats

import cats.Applicative

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

class TraverseTest extends FlatSpec with Matchers {

  "General traverse" should "look like this" in {
    trait Traverse[F[_]] {
      def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    }

    //e.g. F=List, G=Option

//    trait Traverse2 {
//      def traverse[A, B](fa: List[A])(f: A => Option[B]): Option[List[B]] = {
//        val emptyList = List.empty[B]
//        fa.foldLeft(None: Option[List[B]]) { (acc, a) ⇒
//          acc
//          f(a)
//        }
//      }
//    }
  }

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

  "abc" should "def" in {

    import scala.util.Try

    val listOfTries: List[Try[String]] = List(Try("a"), Try("b"), Try("c"))

    // I have a List[Try[String]] but I actually want a Try[List[String]].
    // I want it to be a Failure if any of the individual Trys was a Failure.
    // Traverse to the rescue!

    import cats.Traverse

    // This brings in an instance of Applicative for Try
    import cats.instances.try_._

    // This brings in an instance of Traverse for List
    import cats.instances.list._

    val tryOfList: Try[List[String]] = Traverse[List].sequence(listOfTries)

    // Job done! Or, with a bit of syntax sugar...

    import cats.syntax.traverse._

    val tryOfList2: Try[List[String]] = listOfTries.sequence

    // As pointed out by @philwills, the `traverse` method is also useful:
    val listOfStrings = List("1", "2", "3")

    val tryOfList3: Try[List[Int]] = listOfStrings.traverse(x => Try(x.toInt))

    //------------------
    val listOfEithers: List[Either[String, Int]] = List(Right(1), Right(2))

    import cats.instances.either._

    val eitherOfList: Either[String, List[Int]] = Traverse[List].sequence(listOfEithers)

    val eitherOfList2: Either[String, List[Int]] = listOfEithers.traverse(x => x)

    eitherOfList shouldBe eitherOfList2
    eitherOfList2 shouldBe Right(List(1, 2))

    List(Right(1), Left("2"), Right(3)).traverse(x => x) shouldBe Left("2")

  }

  "listTraverse and listSequence" should "work" in {

//    {
//      import cats.Semigroup
//      import cats.data._
//      import cats.implicits._
//      import cats.data.EitherT
//      import scala.language.higherKinds
//      import cats.syntax.applicative._ // for pure
//      import cats.syntax.apply._ // for mapN
//      import cats.instances.list._ // for Applicative
//      import cats.instances.try_._
//      import cats.syntax.traverse._
//      import cats.Traverse
//
//      def parseIntEither(s: String): EitherT[List, NumberFormatException, Int] =
//        EitherT(List(Either.catchOnly[NumberFormatException](s.toInt)))
//      def parseIntEither2(s: String): Either[NumberFormatException, Int] =
//        Either.catchOnly[NumberFormatException](s.toInt)
//
//      val listOfTries: List[Try[String]] = List(Try("a"), Try("b"), Try("c"))
//      val tryOfList: Try[List[String]] = Traverse[List].sequence(listOfTries)
//
//      //Traverse[List].traverse(List(Option(1), None, Option(2)))(Some(_)) shouldBe 1
//
////      List("1", "2", "3").traverse(parseIntEither(_)) shouldBe Right(List(1, 2, 3))
//////      List("1", "abc", "3").traverse(parseIntEither).isLeft shouldBe List.empty[Int]
//
//    }

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
