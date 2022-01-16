package FPScala

import scala.annotation.tailrec
import scala.math.sqrt

// Exercises from https://www.scala-exercises.org/scala_tutorial/tail_recursion
object TailRecursionExercises extends App {

  // Euclid's Algorithm
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a else {
      gcd(b, a % b)
    }
  }

  //println(gcd(153, 27))

  // Factorial
  def factorial(n: Int): BigInt = {
    @tailrec
    def accFactorial(m: Int, acc: BigInt): BigInt = {
      if (m <= 0) acc
      else accFactorial(m - 1, acc * m)
    }
    accFactorial(n, 1)
  }
  //println(factorial(60))


  // FindFirstElement in List
  // First concrete approach

//  def findFirst(ss: Array[String], key: String): Int = {
//    @tailrec
//    def loop(n: Int): Int = {
//      if (n >= ss.length) -1
//      else if (ss(n) == key) n
//      else loop(n + 1)
//    }
//    loop(0)
//  }

  // more generic approach
  def findFirst[A](ss: Array[A], predicate: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int = {
      if (n >= ss.length) -1
      else if (predicate(ss(n))) n
      else loop(n + 1)
    }
    loop(0)
  }
  val myArray = Array("Danijel", "Zivoi", "ist", "aktuell", "krank")
  val idx = findFirst(myArray, (x: String) => x == "krank")

  println(idx)
  println(myArray(idx))


  // Exercise 2.2 from FP in Scala: Implement isSorted


  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(m: Int, acc: Boolean): Boolean = {
      if (m >= as.length | acc) acc
      else loop(m + 1, acc & ordered(as(m), as(m + 1)))
    }
    loop(0, true)
    /*
    as = Array(1,3,2,5), ordered: (a, b) => a <= b
    ss.length = 4

    loop(0, true) =
      loop(1, true & ordered(as(0), as(1)) = loop(1, true) = // m = 1
      loop(2, true & ordered(as(1), as(2)) = loop(2, false) =  // m = 2
      acc = false
    */
  }
  val temp = Array(1,2,4,5)

  def someFunction = () => "Hello, World"

  println(someFunction)


  //Exercise: isPrime tailrecursive
  def isPrime(n: BigInt): Boolean = {
    @tailrec
    def isPrimeUntil(t: BigInt): Boolean = {
      t == 1 || t > 1 && n % t != 0 && isPrimeUntil(t-1)
    }
    isPrimeUntil(sqrt(n.toDouble).toInt)
  }
  //val p = BigInt("574795249112263276206420860603")
  val p = BigInt("129")
  println(isPrime(p))


  // Implement Maximum
  def myOwnMaximum[A](as: List[A])(implicit ordered: Ordering[A]): A = as.max















}
