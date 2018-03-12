package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}
import cats.Foldable

class FoldableTest extends FlatSpec with Matchers {

  "Built-in Foldable" should "work" in {

    {
      import cats.instances.list._ // for Foldable
      Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) shouldBe 6
    }

    {
      import cats.instances.option._ // for Foldable
      Foldable[Option].foldLeft(Option(3), 10)(_ + _) shouldBe 13
    }
  }
}
