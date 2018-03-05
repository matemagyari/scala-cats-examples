package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq

class EitherExamplesTest extends FlatSpec with Matchers {

  "Creating an either instance" should "work" in {

    import cats.syntax.either._ // for asRight, asLeft

    1.asRight[String] shouldBe Right(1)
    1.asRight[String] shouldBe a[Either[String, Int]]

    "hey".asLeft[Int] shouldBe Left("hey")
    "hey".asLeft[Int] shouldBe a[Either[String, Int]]

    "hey".asLeft[Int].recover { case x ⇒ -1 } shouldBe Right(-1)
  }

}
