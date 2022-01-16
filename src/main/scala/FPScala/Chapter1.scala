package FPScala

object Chapter1 extends App {

  val ll = List.fill(3)(("cc", 10)) ++ List.fill(2)(("dd", 4)) ++ List.fill(5)(("ee", 2))

  def combine(A: (String, Int), B: (String, Int)) : (String, Int) = {
    if (A._1 == B._1) (A._1, A._2 + B._2) else throw new Exception("not Compatible CC's")
  }

  def coalesce(purchases: List[(String, Int)]): List[(String, Int)] =
    purchases.groupBy(_._1).values.map(_.reduce(combine)).toList


  println((ll))
  println(coalesce(ll))

}
