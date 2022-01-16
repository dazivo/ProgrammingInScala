package FPScala


import scala.annotation.tailrec
import scala.math.sqrt

object Chapter3 extends App {

  /* --> Commented out so that no naming collision with standard library occurs

  // Implementation of singly linked lists

  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    // implementation of sum for Ints
    def sum(ints: List[Int]): Int = {
      @tailrec
      def auxSum(as: List[Int], acc: Int): Int = as match {
          case Nil => acc
          case Cons(x, xs) => auxSum(xs, acc + x)
        }
      auxSum(ints, 0)
    }

    def product(ds: List[Double]): Double = {
      @tailrec
      def auxProduct(as: List[Double], acc: Double): Double = as match {
          case Nil => acc
          case Cons(0.0, _) => 0.0
          case Cons(x, xs) => auxProduct(xs, acc * x)
        }
      auxProduct(ds, 1.0)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    // Exercise 3.2 : Implement tail
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    // Exercise 3.3 : Implement setHead
    def setHead[A](as: List[A], a: A): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => Cons(a, xs)
    }

    // Exercise 3.4 : Implement drop
    @tailrec
    def drop[A](as: List[A], n: Int): List[A] = {
      if (n <= 0) as
      else as match {
        case Nil => Nil
        case Cons(x, xs) => drop(xs, n - 1)
      }
    }

    // Exercise 3.6 : Implement init
    def init[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    // foldRight

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      /* foldRight(List(1,2,3), 0)(f) =
            f(1, foldRight(List(2,3), 0)(f)) =
            f(1, f(2, foldRight(List(3), 0)(f)) =
            f(1, f(2, f(3, foldRight(Nil, 0)(f)) =
            f(1, f(2, f(3,0))
      */

    }
    // implement length
    def length[A](as: List[A]): Int =
      foldRight(as, 0)((a, b) => b + 1)

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
    /*
      foldLeft(List(1,2,3), 0)(f) =
        foldLeft(List(2,3), f(0, 1))(f) =
        foldLeft(List(3), f(f(0,1), 2)(f) =
        foldLeft(Nil, f(f(f(0,1), 2), 3)(f) =
        f(f(f(0, 1), 2), 3)
    */

    def reverse[A](as: List[A]): List[A] =
      foldLeft(as, List[A]())((acc, h) => Cons(h, acc))

    /*
      foldLeft(List(1,2,3), List[A]())( ...) =
        f(f(f(1, List()), 2), 3) =
        f(f(Cons(1, Nil), 2), 3) =
        f(Cons(2, Cons(1, Nil), 3) =
        Cons(3, Cons(2, Cons(1, NIl)))
    */

    def append[A](as: List[A], bs: List[A]): List[A] =
      foldRight(as, bs)(Cons(_, _))
    /*
        foldRight(List(1,2), List(3,4)(Cons(_, _)) =
          f(1, f(2, List(3,4))) =
          Cons(1, Cons(2, List(3,4)))
    */

    def append2[A](as: List[A], bs: List[A]): List[A] =
      foldLeft(reverse(as), bs)((a, b) => Cons(b, a))
    /*
      foldLeft(reverse(List(1,2)), List(3,4))((a, b) => Cons(b, a)) =
        foldLeft(List(2,1), List(3,4))(f) =
        f(f(List(3,4), 2), 1) =
    */

    def flatten[A](as: List[List[A]]): List[A] = {
//      as match {
//      case Nil => Nil
//      case Cons(x, xs) => append(x, flatten(xs))
//    }
    foldLeft(as, Nil: List[A])(append)
    }

    /*
      flatten(List(List(1, 2), List(3, 4))) = List(1,2,3,4)
    */

    // Exercise 3.16

//    def map[A, B](as: List[A])(f: A => B): List[B] = as match {
//      case Nil => Nil
//      case Cons(x, xs) => Cons(f(x), map(xs)(f))
//    }

    def map[A, B](as: List[A])(f: A => B): List[B] = {
      foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))
    }
    /*
      foldRight(List(1,2,3), Nil)(f) =
        f(1, f(2, f(3, Nil))) =
        f(1, f(2, Cons(f(3), Nil)) =
        Cons(f(1), Cons(f(2), Cons(f(3), Nil))
    */

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
      flatten(map(as)(f))
  }



  import List._
  val lsInts= List(1, 2, 3, 4, 5, 6, 7)
  val lsDouble = List(1.0, 2.0, 3.0, 5.0)
  val llInts = List(List(1,2,3), List(4,5,6), List(7, 8, 9))

  println(lsInts)
  println(sum(lsInts))
  println(product(lsDouble))
  println(setHead(lsInts, 0))
  println(drop(lsInts, 5))

  println(init(lsInts))
  println(length(lsInts))
  println(reverse(lsInts))
  println(flatten(llInts))
  println(map(lsInts)( _ * 2))
  */

  // Chapter 3.5: Trees

  sealed trait Tree[+A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


  // small case class for Stock Prices
  type Currency = String
  type Price = Double
  case class StockPrice(currency: Currency, price: Price)

  trait Combinator[A]{
    def combine(a: A, b: A): A
  }

  object Combinator {
    implicit val intCombine: Combinator[Int] = _ + _
    implicit val doubleCombine: Combinator[Double] = _ + _
    implicit val stringCombine: Combinator[String] = _ + _
    implicit val priceCombine: Combinator[StockPrice] = {
      (s: StockPrice, p: StockPrice) => s match {
        case StockPrice(c, price) => StockPrice(c, (price + p.price) / 2)
      }
    }
  }


  object Tree {
    def size[A](tr: Tree[A]): Int =
      tr match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
      }

    def maximum(tr: Tree[Int]): Int =
      tr match {
        case Leaf(n) => n
        case Branch(left, right) => maximum(left) max maximum(right)
      }

    def map[A, B](tr: Tree[A])(f: A => B): Tree[B] =
      tr match {
        case Leaf(a) => Leaf(f(a))
        case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      }

    // collapse method for Double
    def reduce[A](tr: Tree[A])(implicit combinator: Combinator[A]): A = tr match {
      case Leaf(v) => v
      case Branch(left, right) => combinator.combine(reduce(left)(combinator), reduce(right)(combinator))
    }
  }

  import Combinator._

  val BinomialTree: Tree[StockPrice] =
    Branch(Branch(Leaf(StockPrice("SFr", 90)), Leaf(StockPrice("SFr", 110))), Branch(Leaf(StockPrice("SFr", 120)), Leaf(StockPrice("SFr", 150))))


  val LargeTree: Tree[Double] = {
    val rn = scala.util.Random
    def loop(m: Int): Tree[Double] = {
      if (m <= 1) Leaf(rn.nextDouble)
      else Branch(loop(m - 1), loop(m - 1))
    }

    loop(4)
  }

  println(LargeTree)
  println(Tree.reduce(LargeTree))


  println(BinomialTree)
  println(Tree.reduce(BinomialTree))
//
}
