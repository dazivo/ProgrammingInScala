package ScalaPuzzlers

object Puzzler1 extends App {

  lazy val variant1: List[Int] = List(1,2).map { i => {println("Hi"); i + 1} }
  lazy val variant2: List[Int] = List(1,2).map { println("Hi"); _ + 1}

  println(variant1)
  println("-" * 10)
  println(variant2)

}
