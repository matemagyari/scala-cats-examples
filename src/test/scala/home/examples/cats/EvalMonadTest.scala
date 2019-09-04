package home.examples.cats

import java.util.concurrent.atomic.AtomicInteger

import cats.Eval
import org.scalatest.{FlatSpec, Matchers}

class EvalMonadTest extends FlatSpec with Matchers {

  "Eval" should "work" in {

    val x1 = new AtomicInteger(0)
    val x2 = new AtomicInteger(0)
    val x3 = new AtomicInteger(0)

    val e1: Eval[Int] = Eval.now {
      x1.incrementAndGet()
      1
    }

    val e2: Eval[Int] = Eval.later {
      x2.incrementAndGet()
      1
    }

    val e3: Eval[Int] = Eval.always {
      x3.incrementAndGet()
      1
    }

    x1.get() shouldBe 1
    x2.get() shouldBe 0
    x3.get() shouldBe 0

    e2.value //invoke
    x2.get() shouldBe 1
    e2.value //invoke
    x2.get() shouldBe 1

    e3.value //invoke
    x3.get() shouldBe 1
    e3.value //invoke
    x3.get() shouldBe 2

  }

  "Stack safe factorial with Eval" should "work" in {
    def factorial(n: Int): Int = if (n == 1) 1 else n * factorial(n - 1)

    def factorial2(n: Int): Eval[Int] =
      if (n == 1)
        Eval.now(1)
      else
        Eval.defer { factorial2(n - 1).map(_ * n) }

    factorial(5) shouldBe 120
    factorial2(5).value shouldBe 120
  }

  "Stack safe foldRight with Eval" should "work" in {

    //this is not stack safe
    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B = as match {
      case head :: tail =>
        fn(head, foldRight(tail, acc)(fn))
      case Nil =>
        acc
    }

    def foldRight2[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] = as match {
      case head :: tail =>
        Eval.defer {
          foldRight2(tail, acc)(fn).map(x ⇒ fn(head, x))
        }
      case Nil =>
        Eval.now(acc)
    }

//    foldRight((1 to 50000).toList, 0)(_ + _) //would be stack overflow
    foldRight2((1 to 50000).toList, 0)(_ + _).value shouldBe 1250025000

  }

}
