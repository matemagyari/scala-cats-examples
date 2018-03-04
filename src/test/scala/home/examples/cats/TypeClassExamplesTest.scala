package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

class TypeClassExamplesTest extends FlatSpec with Matchers {

  trait Show[A] {
    def show(value: A): String
  }

  object ShowInstances {
    implicit val intShow = new Show[Int] {
      override def show(value: Int) = value.toString
    }
    implicit val booleanShow = new Show[Boolean] {
      override def show(value: Boolean) = if (value) "True" else "False"
    }
  }

  "Type class" should "work with Interface Object" in {

    object Show {
      def show[A](a: A)(implicit s: Show[A]): String = s.show(a)
    }

    import ShowInstances._

    Show.show(1) shouldBe "1"
    Show.show(false) shouldBe "False"
  }

  it should "work with Interface Syntax" in {

    object ShowSyntax {
      implicit class ShowOps[A](a: A) {
        def show(implicit show: Show[A]) = show.show(a)
      }
    }

    import ShowInstances._
    import ShowSyntax._

    1.show shouldBe "1"
    false.show shouldBe "False"
  }

  it should "scale" in {

    object Show {
      def show[A](a: A)(implicit s: Show[A]): String = s.show(a)
      def show[A](a: Option[A])(implicit s: Show[A]): String = a.map(s.show(_)).getOrElse("")
    }

    import ShowInstances.intShow

    Show.show(1) shouldBe "1"
    Show.show(Some(1)) shouldBe "1"
    Show.show(None) shouldBe ""
  }
}
