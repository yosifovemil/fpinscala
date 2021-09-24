package fpinscala.datastructures


object DataStructureTests {

  import List._
  import Tree._

  def formatResult[A](fname: String, input: List[A], output: List[A]) = {
    val msg = "%s of %s is %s"
    msg.format(fname, input.toString, output.toString)
  }

  def printHeader(exercise: String): Unit = {
    println(s"########## Exercise: $exercise ##########")
  }

  def main(args: Array[String]): Unit = {
    val testList = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    val testDoubleList = Cons(1.0, Cons(2.0, Cons(3.0, Nil)))
    val nilList = Nil

    // Exercise 3.2
    val testListTail = tail(testList)
    println(formatResult("tail", testList, testListTail))
    try {
      val nilListTail = tail(nilList)
      println(formatResult("tail", nilList, nilListTail))
    } catch {
      case _: NoSuchElementException => println("Tail of empty list correctly throws exception")
      case e: Throwable => e.printStackTrace()
    }

    // Exercise 3.3
    val testListSetHead = setHead(testList, 50)
    println(formatResult("setHead", testList, testListSetHead))
    val nilListSetHead = setHead(nilList, 100)
    println(formatResult("setHead", nilList, nilListSetHead))

    // Exercise 3.4
    val testListDrop = drop(testList, 3)
    println(formatResult("drop 3", testList, testListDrop))
    try {
      val nilListTail = drop(nilList, 2)
      println(formatResult("drop 2", nilList, nilListTail))
    } catch {
      case _: NoSuchElementException => println("drop 2 out of 0 elements correctly throws exception")
      case e: Throwable => e.printStackTrace()
    }

    try {
      val testListDropError = drop(testList, 10)
      println(formatResult("drop 10", nilList, testListDropError))
    } catch {
      case _: NoSuchElementException => println("drop 10 out of 5 elements correctly throws exception")
      case e: Throwable => e.printStackTrace()
    }

    //Exercise 3.5
    val testListDropWhile = dropWhile[Int](testList, x => x <= 3)
    println(formatResult("dropWhile x <= 3", testList, testListDropWhile))
    val testListDropWhile10 = dropWhile[Int](testList, x => x <= 10)
    println(formatResult("dropWhile x <= 10", testList, testListDropWhile10))
    val nilListDropWhile= dropWhile[Int](nilList, x => x < 0)
    println(formatResult("dropWhile x <= 0", nilList, nilListDropWhile))

    // Exercise 3.6
    val testListInit = init[Int](testList)
    println(formatResult("init", testList, testListInit))

    // Exercise 3.8
    printHeader("3.8")
    val result = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(s"Result is $result")

    printHeader("3.9")
    println(s"length of $testList is ${length(testList)}")

    printHeader("3.11")
    println(s"lengthLeft of $testList is ${lengthLeft(testList)}")
    println(s"prodLeft of $testList is ${productLeft(testDoubleList)}")
    println(s"sumLeft of $testList is ${sumLeft(testList)}")

    printHeader("3.12")
    println(s"reverseList of $testList is ${reverseList(testList)}")

    printHeader("3.14")
    println(s"append $testList to $testList is ${appendViaFoldRight(testList, testList)}")

    printHeader("3.25")
    val tree = Branch(
      Branch(Leaf(1), Leaf(2)),
      Branch(
        Branch(Leaf(203), Leaf(4)),
        Leaf(13)
      )
    )
    println(s"size of $tree is ${size(tree)}")

    printHeader("3.26")
    println(s"max of $tree is ${maximum(tree)}")

    printHeader("3.27")
    println(s"depth of $tree is ${depth(tree)}")

    printHeader("3.28")
    println(s"map +2 of $tree is ${mapTree(tree)(_ + 2)}")

    printHeader("3.29")
    println(s"sizeViaFold of $tree is ${sizeViaFold(tree)}")
    println(s"maxViaFold of $tree is ${maximumViaFold(tree)}")
    println(s"depthViaFold of $tree is ${depthViaFold(tree)}")
  }
}
