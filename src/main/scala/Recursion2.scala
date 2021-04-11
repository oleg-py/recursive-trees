import cats.data.Nested
import monix.eval.Task
import monix.reactive.Observable


object Recursion2 {
  case class Recursive[A](root: A, children: Observable[Recursive[A]]) {
    def prune(depth: Int): Recursive[A] =
      if (depth <= 0) Recursive(root, Observable.empty[Recursive[A]])
      else Recursive(root, children.map(_.prune(depth - 1)))


    def merge: Observable[A] = {
      Observable(root) ++ children.mergeMap(_.merge)
    }
  }

  object Recursive {
    def unfold[A](f: A => Observable[A])(root: A): Recursive[A] =
      Recursive(root, f(root).map(unfold(f)))
  }
}
