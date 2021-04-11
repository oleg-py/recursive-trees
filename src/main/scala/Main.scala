import cats.effect.ExitCode
import monix.eval.{Task, TaskApp}
import cats.implicits._
import Logic._
import Recursion3._
import cats.effect.concurrent.Ref
import cats.effect.implicits._
import monix.reactive.Observable
import scala.util.chaining._

import cats.data.Nested


object Main extends TaskApp {
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

//    Observable.fromTask(fetchPageCached("https://en.wikipedia.org/wiki/Recursion"))
//      .flatMap { root =>
//        Recursive.unfold[Observable, WikiPage] { wp =>
//          Observable.fromIterable(wp.children.map(_.url))
//            .mapParallelUnordered(64)(fetchPageCached)
//        }(root)
//          .transform[String] { case (a, tl) => a.link.title -> tl }
//          .prune(4)
//          .pipe(Recursive.mergeRec)
//      }
//      .mapEval(printL)
//      .completedL
//      .as(ExitCode.Success)

    type Tasks[A] = Nested[List, Task, A]
    fetchPageCached("https://en.wikipedia.org/wiki/Recursion")
      .flatMap { root =>
        Recursive.unfold[Tasks, WikiPage] { wp =>
          wp.children.map(_.url).map(fetchPageCached).nested
        }(root)
          .transform[String] { case (a, tl) => a.link.title -> tl }
          .prune(2)
//          .pipe(Recursive.mergeWith(Task.parSequence(_)))
          .translate[Observable](tasks => Observable.fromIterable(tasks.value).mapParallelUnordered(4)(task => task))
          .pipe(Recursive.mergeRec[String])
          .mapEval(printL)
          .completedL
      }
      .as(ExitCode.Success)
  }
}
