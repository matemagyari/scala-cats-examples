package home.examples.cats

import cats.data.Kleisli
import cats.implicits._

object KleisliExamples extends App {

  def convert(s: String): Option[Int] = if (s.forall(_.isDigit)) Some(s.toInt) else None

  def reciprocal(i: Int): Option[Double] = if (i == 0) None else Some(1.toDouble / i.toDouble)

  val result: Option[Double] = convert("12").flatMap(reciprocal)

  val strToDouble: (String) ⇒ Option[Double] = Kleisli(reciprocal).compose(Kleisli(convert)).run

  strToDouble("435")
}
