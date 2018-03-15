package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class MapReduceTest extends FlatSpec with Matchers {

  "Naive implementation" should "look like this" in {

    import cats.Monoid
    import cats.syntax.semigroup._ // for |+|

    def foldMap[A, B: Monoid](as: List[A])(func: A ⇒ B): B =
      as.foldLeft(Monoid[B].empty) { (acc, a) ⇒
        acc |+| func(a)
      }

    {
      import cats.instances.string._ // for Monoid
      foldMap(List(1, 2))(_.toString) shouldBe "12"
    }

    {
      import cats.instances.list._ // for Monoid
      foldMap(List(1, 2))(List(_)) shouldBe List(1, 2)
    }

    def parallelFoldMap[A, B: Monoid](as: List[A])(func: A ⇒ B): Future[B] = {
      // Calculate the number of items to pass to each CPU:
      val numCores = Runtime.getRuntime.availableProcessors
      val groupSize = (1.0 * as.size / numCores).ceil.toInt

      val groups: List[List[A]] = as.grouped(groupSize).toList

      import cats.instances.future._ // for Applicative
      import cats.instances.list._ // for Traverse
      import cats.syntax.traverse._ // for traverse

      def futureFoldMap(list: List[A]): Future[B] = Future {
        foldMap(list)(func)
      }

      groups.traverse(futureFoldMap).map { list ⇒
        list.foldLeft(Monoid[B].empty)(_ |+| _)
      }
    }

    val input = (1 to 9999999).toList
    def func(n: Int) = Math.sqrt(n)
    import cats.instances.double._ // for Monoid
    val parallelResult: Future[Double] = parallelFoldMap(input)(func)
    val simpleResult: Double = foldMap(input)(func)

//    await(parallelResult).toInt shouldBe 666616
//    simpleResult.toInt shouldBe 666616

    runAndMeasureTime {
      parallelFoldMap(input)(func)
    }

    runAndMeasureTime {
      foldMap(input)(func)
    }

  }

  private def await[A](f: Future[A]): A = Await.result(f, 2 seconds)

  private def runAndMeasureTime[A](body: ⇒ A) = {
    //warm up
    (1 to 10).foreach { _ ⇒
      body
    }
    def time() = System.currentTimeMillis()
    val start = time()
    val result = body
    println(s"Time ${time() - start}")
    result
  }
}
