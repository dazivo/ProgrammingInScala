package FPScala

import scala.annotation.tailrec

object Chapter2 {

  // Exercise 2.1 Tail Recursive Fibonacci
  def fibonacci(n: Int): Int = {
    @tailrec
    def accFibonacci(a: Int, b: Int, acc: Int): Int = {
      if (acc <= 1) a
      else accFibonacci(a + b, a, acc - 1)
    }

    accFibonacci(1, 1, n)
  }
  //  println(fibonacci(3))


  // Exercises 2.3 - 2.5
  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = x => f(a, x)
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = (a, b) => f(a)(b)
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
  
  
}


