package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}
import cats.Show
import cats.instances.int._ // for Show
import cats.instances.string._ // for Show
import cats.syntax.show._ // for show

class ShowTest extends FlatSpec with Matchers {

  final case class Cat(name: String, age: Int)

  "Cats' own Show typeclass" should "work" in {

    implicit val catShow = Show.show[Cat] { cat ⇒
      s"${cat.name.show}, ${cat.age.show}"
    }

    Cat("Tobermory", 6).show shouldBe "Tobermory, 6"

  }

}
