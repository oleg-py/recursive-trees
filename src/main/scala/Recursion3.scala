import Recursion1.Tasks
import cats.{Applicative, Functor, Monad, MonoidK}
import monix.reactive.Observable
import cats.implicits._
import monix.eval.Task

object Recursion3 {
  case class Recursive[F[_], A](root: A, children: F[Recursive[F, A]]) {
    def prune(depth: Int)(implicit F: Functor[F], F2: MonoidK[F]): Recursive[F, A] = {
      if (depth <= 0) Recursive(root, F2.empty[Recursive[F, A]])
      else Recursive(root, children.map(_.prune(depth - 1)))
    }

    def zipWithDepth(implicit F: Functor[F]): Recursive[F, (A, Int)] = {
      def run(r: Recursive[F, A], depth: Int): Recursive[F, (A, Int)] =
        Recursive(r.root -> depth, children.map(run(_, depth + 1)))

      run(this, 0)
    }

    def translate[G[_]](f: F[Recursive[G, A]] => G[Recursive[G, A]])(implicit F: Functor[F]): Recursive[G, A] = {
      Recursive(root, f(children.map(_.translate(f))))
    }

    def transform[B](f: (A, F[Recursive[F, B]]) => (B, F[Recursive[F, B]]))(implicit F: Functor[F]): Recursive[F, B] = {
      val (nextRoot, nextChildren) = f(root, children.map(_.transform(f)))
      Recursive(nextRoot, nextChildren)
    }
  }

  object Recursive {
    def unfold[F[_]: Functor, A](f: A => F[A])(root: A): Recursive[F, A] =
      Recursive(root, f(root).map(unfold(f)))

    def mergeRec[A](rec: Recursive[Observable, A]): Observable[A] =
      Observable(Observable(rec.root), rec.children.flatMap(mergeRec)).merge

    def mergeWith[A](sequencer: List[Task[List[A]]] => Task[List[List[A]]])(rec: Recursive[Tasks, A]): Task[List[A]] = {
      sequencer(rec.children.value.map(_.flatMap(mergeWith(sequencer))))
        .map(_.flatten)
        .map(list => rec.root :: list)
    }
  }
}
