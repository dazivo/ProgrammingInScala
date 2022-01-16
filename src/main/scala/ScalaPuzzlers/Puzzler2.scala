package ScalaPuzzlers

import javax.print.attribute.standard.JobHoldUntil

object Puzzler2 extends App {

  var MONTH = 12; var DAY = 24;
//  var (HOUR, MINUTE, SECOND) = (12, 0, 0)
  //var (hour, minute, second) = (12, 0, 0)


  case class Person(name: String, age: Int)


  val dani = Person("Danijel", 34)

  val greet: String = dani match {
    case Person(name, _) => s"Ciao ${name}!"
    case _ => s"Ciao Bello!"
  }


  val theanswer = 42

  def checkGuess(guess: Int) = guess match {
    case `theanswer` => "Your guess is correct"
    case _ => "Try again"
  }

  println(checkGuess(42))
  println(checkGuess(21))




}
