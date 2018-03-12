package home.examples.cats

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Try

class ValidatedTest extends FlatSpec with Matchers {

  "Simple Validated" should "look like" in {

    import cats.Semigroupal
    import cats.instances.list._ // for Monoid

    type AllErrorsOr[A] = Validated[List[String], A]

    val result = Semigroupal[AllErrorsOr].product(
      Validated.invalid(List("error 1")),
      Validated.invalid(List("error 2"))
    )

    result shouldBe Invalid(List("error 1", "error 2"))
  }

  "Create Validate" should "look like this" in {

    val v: Valid[Int] = Valid(1)

    v.a shouldBe 1

    val i: Invalid[List[String]] = Invalid(List("Badness"))

    i.e shouldBe List("Badness")

    type ErrorsOr[A] = Validated[List[String], A]

    //using smart constructors
    {
      val v: ErrorsOr[Int] = Validated.valid[List[String], Int](123)
      val i: ErrorsOr[Int] = Validated.invalid[List[String], Int](List("Badness"))
    }

    //using expression methods
    {
      import cats.syntax.validated._ // for valid and invalid

      val v: ErrorsOr[Int] = 1.valid[List[String]]
      val i: ErrorsOr[Int] = List("Badness").invalid[Int]
    }

//    {
//      import cats.syntax.applicative._ // for pure
//      import cats.syntax.applicativeError._ // for raiseError
//
//      val v: ErrorsOr[Int] = 1.pure[ErrorsOr]
//      val i: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
//    }

    {
      //helper methods

      val v1: Validated[Throwable, Int] = Validated.fromTry(scala.util.Try("foo".toInt))
      val v2: Validated[String, Int] = Validated.fromEither[String, Int](Left("Badness"))
      val v3: Validated[String, Int] = Validated.fromOption[String, Int](None, "Badness")
    }

  }

  "Validated" should "turn to Either and back" in {
    import cats.syntax.either._ // for toValidated
    import cats.syntax.validated._ // for valid and invalid

    val i: Validated[String, Int] = "Badness".invalid[Int]

    i.toEither.toValidated shouldBe i
  }

  "Form validation" should "look like this" in {

    import cats.data.Validated
    type FormData = Map[String, String]
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]

    def getValue(name: String)(data: Map[String, String]): FailFast[String] =
      data.get(name).toRight(List(s"$name field not specified"))

    val getName: FormData ⇒ FailFast[String] = getValue("name")

    import cats.syntax.either._ // for catchOnly
    def parseInt(data: String): FailFast[Int] =
      Either
        .catchOnly[NumberFormatException](data.toInt)
        .leftMap(_ ⇒ List(s"$data must be an integer"))

    def nonBlank(name: String)(data: String): FailFast[String] =
      Right(data).ensure(List(s"$name cannot be blank"))(_.nonEmpty)

    def nonNegative(data: Int): FailFast[Int] =
      Right(data).ensure(List(s"$data must be non-negative"))(_ >= 0)

    def readAge(data: FormData): FailFast[Int] =
      getValue("age")(data)
        .flatMap(nonBlank("age"))
        .flatMap(parseInt)
        .flatMap(nonNegative)

    def readName(data: FormData): FailFast[String] =
      getValue("name")(data).flatMap(nonBlank("name"))

    readAge(Map("age" → "12")) shouldBe Right(12)
    readAge(Map("age" → "a")) shouldBe Left(List(s"a must be an integer"))

    case class User(name: String, age: Int)

    import cats.syntax.either._ // for toValidated
    import cats.syntax.validated._ // for valid and invalid
    import cats.instances.list._ // for Semigroupal
    import cats.instances.tuple._ // for Semigroupal
    import cats.syntax.apply._ // for mapN

    //looks like for comprehension
//    def readUser(data: FormData): FailSlow[User] =
//      (readName(data).toValidated, readAge(data).toValidated).mapN { (n: String, a: Int) ⇒
//        User(n, a)
//      }

//    readUser(Map()) shouldBe Left("age should not be blank")
  }

  "ValidatedNel usage" should "look like this" in {
    import cats.data.{ValidatedNel, NonEmptyList}
    import cats.implicits._
    type ValidationResult[A] = ValidatedNel[String, A]

    def validName(name: String): ValidationResult[String] =
      if (name.isEmpty) "Empty name".invalidNel else name.validNel

    def validAge(age: Int): ValidationResult[Int] =
      if (age < 1) "Invalid age".invalidNel else age.validNel

    case class Person(name: String, age: Int)

    def tryValues(name: String, age: Int): ValidationResult[Person] =
      (validName(name), validAge(age)).mapN(Person.apply)

    tryValues("joe", 18) shouldBe Valid(Person("joe", 18))
    tryValues("joe", -1) shouldBe Invalid(NonEmptyList.of("Invalid age"))
    tryValues("", 2) shouldBe Invalid(NonEmptyList.of("Empty name"))
    tryValues("", -1) shouldBe Invalid(NonEmptyList.of("Empty name", "Invalid age"))
  }
}
