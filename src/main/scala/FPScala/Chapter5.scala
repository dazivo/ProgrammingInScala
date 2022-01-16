package FPScala

object Chapter5 extends App {

  sealed trait Stream[+A] {

    final def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }
    final def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
    // Implementation 1
//    @annotation.tailrec
//    final def exists(p: A => Boolean): Boolean = this match {
//      case Empty => false
//      case Cons(h, t) => p(h()) || t().exists(p)
//    }

    final def foldRight[B](z: B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

    final def exists(p: A => Boolean): Boolean =
      this.foldRight(false)((a, b) => p(a) || b)
  }

  case object Empty extends Stream[Nothing]

  final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl

      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }

  }

  val stream1: Stream[Int] = Stream(1, 2, 3)
  println(stream1.headOption)
  println(stream1.toList)



}
