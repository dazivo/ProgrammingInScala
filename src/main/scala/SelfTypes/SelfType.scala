package SelfTypes

/*
  https://www.youtube.com/watch?v=ZnpB4B1fMSI
*/
object SelfType extends App {

  trait Edible

  trait Person {
    def hasAllergiesTo(thing: Edible): Boolean
  }

  trait Child extends Person
  trait Adult extends Person

  trait Diet {self: Person =>
    def eat(thing: Edible): Boolean = {
      if (self.hasAllergiesTo(thing)) false
      else 42 > 10
    }
  }

  trait Carnivore extends Diet with Person
  trait Vegetarian extends Diet with Person

  val temp: Child = new Child {
    override def hasAllergiesTo(thing: Edible): Boolean = ???
  }


}
