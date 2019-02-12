package cats.traverse.example

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import cats.instances.future._
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{Applicative, Id}

object Main extends App {

  // -------- Standard library's Future has sequence and traverse functions --------

  def compute(s: String): Int = {
    Thread.sleep(100)
    s.length
  }

  val abFuture: List[Future[Int]] =
    List(Future(compute("a")), Future(compute("ab")))

  // - start with a List[Future[A]]
  // - end up with aFuture[List[A]]
  val abSequenced: Future[List[Int]] =
  Future.sequence[Int, List](abFuture)

  val abList: List[String] = List("a", "ab")
  // - start with a List[A]
  // - provide a function A => Future[B]
  // - end up with a Future[List[B]]
  val abTraversed: Future[List[Int]] =
  Future.traverse[String, Int, List](abList)(s => Future(compute(s + s)))

  // ------------------------------------

  // -------- Very simple example domain --------
  // Basically, the requirement is to verify health checks of dependent services (database, etc.).
  // The result is true when all health checks returned 200, otherwise it's false.

  case class HttpResponse(sc: StatusCode, body: String)
  case class StatusCode(code: Int)

  case class Url(url: String)

  def healthCheckFuture(url: Url): Future[HttpResponse] =
    Future(HttpResponse(StatusCode(200), "appId=xxx"))

  def allHealthy(responses: List[HttpResponse]): Boolean = responses.forall(_.sc.code == 200)

  val urls = List(Url("http://raz"), Url("http://dwa"))
  val responses = List(HttpResponse(StatusCode(200), "body1"), HttpResponse(StatusCode(200), "body2"))

  // ------------------------------------

  // -------- Implementation using just Future --------

  def checkResultsFuture(healthCheckResults: List[Future[HttpResponse]]): Future[Boolean] =
    Future.sequence(healthCheckResults).map(allHealthy)

  def askExternalFuture(healthCheckUrls: List[Url]): Future[Boolean] =
    Future.traverse(healthCheckUrls)(healthCheckFuture).map(allHealthy)

  val checkResultsFutureR: Future[Boolean] = checkResultsFuture(responses.map(Future.successful))
  println(Await.result(checkResultsFutureR, 1 second))

  val askExternalFutureR: Future[Boolean] = askExternalFuture(urls)
  println(Await.result(askExternalFutureR, 1 second))


  // -------- Simplified Traverse typeclass --------

//  trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
//    def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
//    def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] =
//  }

  // -------- Traverse usage on the same domain --------

  def checkResultsF[F[_] : Applicative](healthCheckResults: List[F[HttpResponse]]): F[Boolean] =
    healthCheckResults.sequence.fmap(allHealthy)

  def askExternalF[F[_] : Applicative](healthCheckUrls: List[Url], f: Url => F[HttpResponse]): F[Boolean] =
    healthCheckUrls.traverse[F, HttpResponse](f).fmap(allHealthy)

  // Production code

  val askExternalFR: Future[Boolean] = askExternalF(urls, healthCheckFuture)
  println(Await.result(askExternalFR, 1 second))

  // Test code - simpler, without the Future context - using type alias Id instead

  def healthCheckMock(url: Url): HttpResponse = HttpResponse(StatusCode(200), "appId=xxx")

  val checkResultsIdR: Boolean = checkResultsF[Id](responses)
  println(checkResultsIdR)

  val askExternalIdR: Boolean = askExternalF[Id](urls, healthCheckMock)
  println(askExternalIdR)

}
