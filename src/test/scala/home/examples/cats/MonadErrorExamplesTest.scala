package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

class MonadErrorExamplesTest extends FlatSpec with Matchers {

  "MonadError" should "work" in {

    import cats.MonadError
    import cats.instances.either._ // for MonadError

    type ErrorOr[A] = Either[String, A]
    val monadError = MonadError[ErrorOr, String]

    monadError.pure("hey") shouldBe an[ErrorOr[Int]]
    monadError.pure("hey") shouldBe Right("hey")

    val failure = monadError.raiseError("Badness")
    failure shouldBe an[ErrorOr[Nothing]]
    failure shouldBe Left("Badness")

    val error: ErrorOr[ErrorOr[String]] = monadError.handleError(failure) {
      case "Badness" ⇒ monadError.pure("It's ok")
      case _ ⇒ monadError.raiseError("It's not ok")
    }
  }

}
