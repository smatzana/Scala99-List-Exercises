package com.matzanas.S99

import scala.annotation.tailrec


object P01 {
  /*
  P01 (*) Find the last element of a list.
  Example:
  scala> last(List(1, 1, 2, 3, 5, 8))
  res0: Int = 8
  */
  @tailrec def last[T](aList: List[T]) :T =
    aList match {
      case h :: Nil => h
      case _ :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }

}

object P02 {

  /*
    P02 (*) Find the last but one element of a list.
    Example:
    scala> penultimate(List(1, 1, 2, 3, 5, 8))
    res0: Int = 5
   */
  @tailrec def penultimate[T](aList: List[T]) : T =
    aList match {
      case h :: List(_) => h
      case _ :: tail => penultimate(tail)
      case _ =>  throw new NoSuchElementException
    }
}

object S99 extends App {

  val aList = List(1, 1, 2, 3, 5, 8)
  println(s"P01 Last element : ${P01.last(aList)}")
  println(s"P02 Penultimate element : ${P02.penultimate(aList)}")


}