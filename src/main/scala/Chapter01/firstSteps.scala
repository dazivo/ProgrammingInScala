package Chapter01

import scala.util.Sorting

object firstSteps extends App {

  val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))

  Sorting.quickSort(pairs)(Ordering.by[(String, Int, Int), Int](_._2))

}
