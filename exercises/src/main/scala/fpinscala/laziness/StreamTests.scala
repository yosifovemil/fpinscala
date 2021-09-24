package fpinscala.laziness

object StreamTests {
  import Stream._

  def printHeader(exercise: String): Unit = {
    println(s"########## Exercise: $exercise ##########")
  }

  def main(args: Array[String]): Unit = {
    val stream = Stream({println("ONE"); 1}, {println("TWO"); 2}, {println("THREE"); 3})
    printHeader("5.1")
    println(s"toList of ${stream} is ${stream.toList}")


    val stream2 = Stream(1,2,3,4,5,6,7,8,9,10)
    printHeader("5.2 - take")
    println(s"take(5) of ${stream2.toList} is ${stream2.take(5).toList}")

    val stream3 = Stream(1,2,3,4)
    try {
      println(s"take(5) of ${stream.toList} is ${stream3.take(5).toList}")
    } catch {
      case e: Exception => {
        println(s"take(5) of ${stream.toList} throws ${e}")
      }
    }


    printHeader("5.2 - drop")
    println(s"drop(5) of ${stream2.toList} is ${stream2.drop(5).toList}")
    try {
      println(s"drop(5) of ${stream.toList} is ${stream3.drop(5).toList}")
    } catch {
      case e: Exception => {
        println(s"drop(5) of ${stream.toList} throws ${e}")
      }
    }

    printHeader("5.3")
    println(s"${stream2.toList}.takeWhile(_ < 4) = ${stream2.takeWhile(_ < 4).toList}")
    println(s"${stream3.toList}.takeWhile(_ > 10) = ${stream3.takeWhile(_ > 10).toList}")

    printHeader("5.4")
    println(s"${stream2.toList}.forAll(_ < 11) = ${stream2.forAll(_ < 11)}")
    println(s"${stream3.toList}.forAll(_ < 3) = ${stream3.forAll(_ < 3)}")
    println(s"${empty[Int].toList}.forAll(_ > 10) = ${empty[Int].forAll(_ > 10)}")

    printHeader("5.5")
    println(s"${stream2.toList}.takeWhileFR(_ < 4) = ${stream2.takeWhileFR(_ < 4).toList}")
    println(s"${stream3.toList}.takeWhileFR(_ > 10) = ${stream3.takeWhileFR(_ > 10).toList}")

    printHeader("5.7 - map")
    println(s"${stream2.toList}.map(_ + 4) = ${stream2.map(_ + 4).toList}")
    println(s"${stream3.toList}.map(_ * 10) = ${stream3.map(_ * 10).toList}")

    printHeader("5.7 - filter")
    println(s"${stream2.toList}.filter(_ != 4) = ${stream2.filter(_ != 4).toList}")
    println(s"${stream3.toList}.filter(_ < 3) = ${stream3.filter(_ < 3).toList}")

    printHeader("5.7 - append")
    println(s"${stream2.toList}.append(${stream3.toList}) = ${stream2.append(stream3).toList}")

    printHeader("5.8")
    val threes = constant(3)
    println(s"constant(3).take(5) = ${threes.take(5).toList}")
  }

}