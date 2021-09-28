package fpinscala.state
import fpinscala.state.RNG._

object StateTests {
  import State._

  def printHeader(exercise: String): Unit = {
    println(s"########## Exercise: $exercise ##########")
  }

  def main(args: Array[String]): Unit = {
    printHeader("6.4")
    val rng = Simple(1000L)
    println(s"ints(10)(rng) = ${ints(10)(rng)}")
  }

}