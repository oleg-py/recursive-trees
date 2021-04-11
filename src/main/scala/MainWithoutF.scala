import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import cats.implicits._
import Logic._
import Recursion2._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import monix.reactive.Observable
import scala.util.chaining._


object MainWithoutF extends TaskApp {
  def printL[A](a: A): Task[Unit] = Task(println(a))

  override def run(args: List[String]): Task[ExitCode] = {
    val fetchPageCached = {
      val cached = Ref.unsafe[Task, Map[String, Task[WikiPage]]](Map())

      (url: String) =>
        cached.modify { map =>
          val task = fetchPage(url).memoize
          map.get(url).fold(map.updated(url, task) -> task)(map -> _)
        }.flatten
    }

    Observable.fromTask(fetchPageCached("https://en.wikipedia.org/wiki/Recursion"))
      .flatMap { root =>
        Recursive.unfold[WikiPage] { wp =>
          Observable.fromIterable(wp.children.map(_.url))
            .mapParallelUnordered(4)(fetchPageCached)
        }(root)
          .prune(1)
          .merge
      }
      .completedL
      .as(ExitCode.Success)
  }
}
