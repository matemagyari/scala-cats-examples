package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

//a Kleisli is function composition for Monads.
class KleisliTest extends FlatSpec with Matchers {

  "Compose with Kleisli" should "work" in {

    def parseInt(s: String): Option[Int] =
      if (s.forall(_.isDigit)) Some(s.toInt) else None

    def reciprocal(i: Int): Option[Double] =
      if (i == 0) None else Some(1.toDouble / i.toDouble)

    //with Kleisli
    import cats.data.Kleisli
    import cats.instances.option._ // for Monad

    val func2: String ⇒ Option[Double] = Kleisli(parseInt) andThen Kleisli(reciprocal) run
    //simple way
    def func(s: String): Option[Double] = parseInt(s).flatMap(reciprocal)

    func("50") shouldBe func2("50")

  }

}
