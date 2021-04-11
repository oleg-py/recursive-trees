import Logic.WikiPage
import monix.eval.Task
import cats.data.Nested
import cats.implicits._

object Recursion1 {
  type Tasks[A] = Nested[List, Task, A]
  case class Recursive[A](root: A, children: Tasks[Recursive[A]]) {
    def prune(depth: Int): Recursive[A] =
      if (depth <= 0) Recursive(root, List.empty[Task[Recursive[A]]].nested)
      else Recursive(root, children.map(_.prune(depth - 1)))


    def toListWith(sequencer: List[Task[List[A]]] => Task[List[List[A]]]): Task[List[A]] = {
      sequencer(children.value.map(_.flatMap(_.toListWith(sequencer))))
        .map(_.flatten)
        .map(list => root :: list)
    }
  }

  object Recursive {
    def unfold[A](f: A => Tasks[A])(root: A): Recursive[A] =
      Recursive(root, f(root).map(unfold(f)))
  }
}
