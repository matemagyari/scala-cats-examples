package home.examples.cats

import cats.Foldable
import org.scalatest.{FlatSpec, Matchers}

class FoldableTest extends FlatSpec with Matchers {

  "Built-in Foldable" should "work" in {

    {
      import cats.instances.list._ // for Foldable
      Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6

      Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))) should be(
        List(1, 2, 3, 4, 5)
      )

      import cats.instances.option._
      Foldable[List].foldK(List(None, Option("two"), Option("three"))) should be(
        Some("two")
      )

    }

    {
      import cats.instances.option._ // for Foldable
      Foldable[Option].foldLeft(Option(3), 10)(_ + _) shouldBe 13
    }

  }

  "Traverse" should "work" in {
    import cats.implicits._

    def parseInt(s: String): Option[Int] =
      Either.catchOnly[NumberFormatException](s.toInt).toOption

    Foldable[List].traverse_(List("1", "2", "3"))(parseInt) should be(
      Some()
    )
    Foldable[List].traverse_(List("a", "b", "c"))(parseInt) should be(None)
  }

  "Foldable" should "compose" in {
    import cats.implicits._

    val FoldableListOption = Foldable[List].compose[Option]
    FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))) should be(10)
    FoldableListOption.fold(List(Option("1"), Option("2"), None, Option("3"))) should be("123")
  }
}
