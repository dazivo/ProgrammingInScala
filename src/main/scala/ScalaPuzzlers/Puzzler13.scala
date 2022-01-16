package ScalaPuzzlers


object Puzzler13 extends App {


  case class Person(name: String){
    def one(int: Int): Int = int + 1
  }

  val dani = Person("Danijel")

  println(dani.one(36))



  def sumItUp: Int = {

    def one(x: Int): Int = { return x; 1}

    val two = (x: Int) => {return x; 2}

    1 + one(2) + two(3)

  }

  println(sumItUp)


}
