package FPScala

import cats.data._
import cats.implicits._

object Chapter7 extends App {

  val list: List[Either[String, Int]] = List(Right(1), Right(2), Right(3), Left("Hey!"))

  val traversed = list.sequence

  println(traversed)


}
