abstract class Hero {
  def die(): Unit
}

trait VitelUpgrade extends Hero {
  abstract override def die(): Unit = {
    println("-- Head")
    super.die()
  }
}

trait MediumUpgrade extends Hero {
  abstract override def die(): Unit = {
    println("Minus Noga")
    super.die()
  }
}

trait CommonUpgrade extends Hero {
  abstract override def die(): Unit = {
    println("Carapka")
    super.die()
  }
}

class FootballHero extends Hero
  with VitelUpgrade
  with MediumUpgrade
  with CommonUpgrade {
  override def die(): Unit = {
    println("Ya loh ya sdoh")
  }
}

object Molecules extends App {
  val hero = new FootballHero

  hero.die()
}
