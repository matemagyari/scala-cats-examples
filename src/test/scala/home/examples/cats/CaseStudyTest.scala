package home.examples.cats

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.Seq
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class CaseStudyTest extends FlatSpec with Matchers {

  "Naive implementation" should "look like this" in {

    trait UptimeClient {
      def getUptime(hostname: String): Future[Int]
    }

    import cats.instances.future._ // for Applicative
    import cats.instances.list._ // for Traverse
    import cats.syntax.traverse._ // for traverse

    class UptimeService(client: UptimeClient) {
      def getTotalUptime(hostnames: List[String]): Future[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
    }

    class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient {
      override def getUptime(hostname: String): Future[Int] = Future(hosts.getOrElse(hostname, 0))
    }

    val service = new UptimeService(new TestUptimeClient(Map("a" → 1, "b" → 2)))

    val result: Future[Int] = service.getTotalUptime(List("a", "b"))
  }

  "Abstracting over type constructors" should "look like this" in {
    import scala.language.higherKinds
    import cats.Id

    trait UptimeClient[F[_]] {
      def getUptime(hostname: String): F[Int]
    }

    class RealUptimeClient extends UptimeClient[Future] {
      override def getUptime(hostname: String): Future[Int] = Future(1)
    }

    class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
      override def getUptime(hostname: String): Int = //could be Id[Int], but not necessary
        hosts.getOrElse(hostname, 0)
    }

    import cats.instances.list._ // for Traverse
    import cats.instances.future._ // for Traverse
    import cats.syntax.traverse._ // for traverse
    import cats.Applicative
    import cats.syntax.functor._ // for map

    class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
      def getTotalUptime(hostnames: List[String]): F[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
    }

    //here we can construct a real instance
    val real = new UptimeService(new RealUptimeClient())
    val realResult: Future[Int] = real.getTotalUptime(List.empty)

    //and a test one
    val test = new UptimeService(new TestUptimeClient(Map("a" → 1, "b" → 2)))
    val testResult: Int = test.getTotalUptime(List("a", "b", "c"))
    testResult shouldBe 3

  }
}
