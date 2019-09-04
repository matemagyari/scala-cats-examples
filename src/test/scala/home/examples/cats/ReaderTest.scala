package home.examples.cats

import java.time.LocalDate

import cats.Id
import org.scalatest.{FlatSpec, Matchers}
import cats.data.{Kleisli, Reader}

class ReaderTest extends FlatSpec with Matchers {

  "Compose with Reader" should "work 2" in {

    case class Context(user: String, systemOn: Boolean)

    def isAuthorised: Reader[Context, Boolean] = Reader[Context, Boolean] { context =>
      context.systemOn && context.user.nonEmpty
    }

    def register(authorized: Boolean): Reader[Context, String] = Reader[Context, String] {
      context =>
        if (authorized) s"${context.user} registered!" else "registration failed"
    }

    val fullComputation: Reader[Context, String] = for {
      authorised <- isAuthorised
      response <- register(authorised)
    } yield response

    fullComputation.run(Context("Joe", false)) shouldBe "registration failed"
    fullComputation.run(Context("Joe", true)) shouldBe "Joe registered!"

  }

  "Compose with Reader" should "work" in {

    val toInt: String ⇒ Int = _.toInt
    val double: Int ⇒ Int = _ * 2

    val toIntReader: Reader[String, Int] = Reader(toInt)
    val doubleReader: Reader[Int, Int] = Reader(double)

    val r4: Reader[String, Int] = doubleReader.compose(toIntReader)
    val r5: Reader[String, Int] = toIntReader.andThen(doubleReader)

    r4.run("1") shouldBe 2
    r4.run("1") shouldBe r5.run("1")

  }

}
