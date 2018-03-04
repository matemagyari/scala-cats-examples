package home.examples.cats

import cats.data.Validated._
import cats.data.{ValidatedNel, _}
import cats.implicits._
import org.scalatest.Matchers

object ValidatedExamples extends App with Matchers {

  type ValidationResult[A] = ValidatedNel[String, A]

  final case class Person(name: String, age: Int)

  object Person {

    def create(name: String, age: Int): ValidationResult[Person] =
      (validName(name), validAge(age)).mapN { (n, a) ⇒
        Person(n, a)
      }

    private def validName(name: String): ValidationResult[String] =
      if (name.isEmpty) "Empty name".invalidNel else name.validNel

    private def validAge(age: Int): ValidationResult[Int] =
      if (age < 1) "Invalid age".invalidNel else age.validNel
  }

  Person.create("", 0) shouldBe Invalid(NonEmptyList.of("Empty name", "Invalid age"))
  Person.create("a", 1) shouldBe Valid(Person("a", 1))
}
