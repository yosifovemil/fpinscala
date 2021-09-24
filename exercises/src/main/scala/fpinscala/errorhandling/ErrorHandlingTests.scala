package fpinscala.errorhandling
import scala.util.Try

object ErrorHandlingTests {
  import Option._

  def printHeader(exercise: String): Unit = {
    println(s"########## Exercise: $exercise ##########")
  }

  def main(args: Array[String]): Unit = {
    val option123:Option[Int] = Some(123)
    val optionNone:Option[Int] = None

    printHeader("4.1")
    println(s"${option123} map (_ * 3) = ${option123.map(_ * 3)}")
    println(s"${optionNone} map (_ * 3) = ${optionNone.map(_ * 3)}")

    println(s"${option123} getOrElse(3) = ${option123.getOrElse(3)}")
    println(s"${optionNone} getOrElse(3) = ${optionNone.getOrElse(3)}")

    println(s"${option123} flatMap(Some(_ * 3)) = ${option123.flatMap(x => Some(x * 3))}")
    println(s"${optionNone} flatMap(Some(_ * 3)) = ${optionNone.flatMap(x => Some(x * 3))}")

    println(s"${option123} orElse(Some(3)) = ${option123.orElse(Some(3))}")
    println(s"${optionNone} orElse(Some(3)) = ${optionNone.orElse(Some(3))}")

    println(s"${option123} filter(_ > 3) = ${option123.filter(_ > 3)}")
    println(s"${optionNone} filter(_ > 3) = ${optionNone.filter(_ > 3)}")
    println(s"${Some(2)} filter(_ > 3) = ${Some(2).filter(_ > 3)}")

    printHeader("4.4")
    val list:List[Option[Int]] = List(Some(1), Some(2), Some(3))
    val listWithNone: List[Option[Int]] = List(Some(1), None, Some(4))
    println(s"sequence of ${list} = ${sequence(list)}")
    println(s"sequence of ${listWithNone} = ${sequence(listWithNone)}")

    printHeader("4.5")
    val stringsToInts = List("1", "30", "99", "100")
    val stringsToIntsFail = List("1", "two", "41", "7")

    def convertToOptionInt(s: String): Option[Int] =
      try {
        Some(s.toInt)
      } catch {
        case e: Exception => None
      }

    println(s"traverse of ${stringsToInts} = ${traverse(stringsToInts)(convertToOptionInt)}")
    println(s"traverse of ${stringsToIntsFail} = ${traverse(stringsToIntsFail)(convertToOptionInt)}")
  }
}
