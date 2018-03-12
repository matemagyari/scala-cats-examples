package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Future

class MonadTransformerTest extends FlatSpec with Matchers {

  "Transforming" should "look like this" in {

    import cats.data.OptionT

    // Alias Either to a type constructor with one parameter:
    type ErrorOr[A] = Either[String, A]

    type ErrorOrOption[A] = OptionT[ErrorOr, A]

    import cats.instances.either._
    import cats.syntax.applicative._ // for pure

    1.pure[ErrorOrOption] shouldBe OptionT[ErrorOr, Int](Right(Some(1)))

  }

  "Transforming" should "look like this 2" in {

    import cats.data.{EitherT, OptionT}
    import cats.instances.future._
    import cats.syntax.applicative._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Future

    type FutureEither[A] = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    val c: FutureEitherOption[Int] = for {
      a ← 1.pure[FutureEitherOption]
      b ← 2.pure[FutureEitherOption]
    } yield a + b

    val intermediate: FutureEither[Option[Int]] = c.value

    val stack: Future[Either[String, Option[Int]]] = intermediate.value

    val value: Either[String, Option[Int]] = await(stack)

    value shouldBe Right(Some(3))

    // Mapping over the Either in the stack:
    await(c.value.map(_.map(_ + 1)).value) shouldBe Right(Some(4))

  }

  private def await[A](f: Future[A]): A = {
    import scala.concurrent.Await
    import scala.concurrent.duration._
    Await.result(f, 1.second)
  }

}
