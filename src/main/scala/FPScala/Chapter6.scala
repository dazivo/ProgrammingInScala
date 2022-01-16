package FPScala

object Chapter6 extends App {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    override def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  val rng = SimpleRNG(12)

  val (n1, rng2) = rng.nextInt
  println(n1)
  println(rng2)

  class Foo {
    private var s: Int = 10
    def bar: Int = 12
    val baz: Int = 14
  }

  val instance = new Foo

  println(instance.bar)
  println(instance.baz)
  println(instance.baz)



}
