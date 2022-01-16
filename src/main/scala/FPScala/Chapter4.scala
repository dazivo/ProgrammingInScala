package FPScala

object Chapter4 extends App {

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch {case e: Exception => None}
  }

  def insuranceRateQuote(age: Int, numberOfSpeedTickets: Int): Double = {
    age * 2 + numberOfSpeedTickets
  }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge = Try(age.toInt)
    val optTicket = Try(numberOfSpeedingTickets.toInt)

    map2(optAge, optTicket)(insuranceRateQuote)
  }

  // Exercise 4.3 : Implement map2
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    (a, b) match {
      case (_, None) => None
      case (None, _) => None
      case (Some(v), Some(w)) => Some(f(v, w))
      }
    }
  // Exercise 4.4 : Implement sequence

  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as match {
      case Nil => Some(Nil)
      case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }
  }
  def sequence_1[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))
  }
  /*
    List(1,2,3).foldRight(Some(Nil))(F(_, _)) =
      F(1, F(2, F(3, Some(Nil)))) =
      F(1, F(2, Some(List(3)))) =
      F(1, Some(List(2, 3))) =
      Some(List(1, 2, 3)))
  */

  val llList = List(Some(4), None, None, Some(2), Some(10))
  println(sequence(llList))



//  val temp: List[Int] = 1 :: 2 :: 3 :: Nil
//
//  println(temp(1))
//
//  lazy val fibs: LazyList[Int] = 0 #:: 1 #:: fibs.zip(fibs.tail).map { n => n._1 + n._2 }
//
//  println( fibs(20) )


  }

