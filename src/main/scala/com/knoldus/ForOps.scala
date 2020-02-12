package com.knoldus

class ForOps {
  //Find the last element of list with its index value(dont use inbuilt methods to extract last element directly)

  def lastTerm() = {
    val lst = List(1, 2, 3, 4)
    for {
      elementIndex <- 0 until lst.length
      if (elementIndex == lst.length - 1)
    } yield {
      (elementIndex, lst(elementIndex))
    }
  }

  //print the table of each element in the List
  def table(): List[Int] = {
    val lst = List(1, 2, 3, 4)
    for {
      a <- lst
      b <- 1 to 10
    } yield {
      a * b
    }
  }

  //aggregate the contents of two lists of same size into a single list
  //List(1,2) and List("a", "b") results List(List(1, "a"), List(2, "b"))

  def mergeList() = {
    val a = List(1, 2)
    val b = List("a", "b")
    val z = (for {
      x <- 0 until a.length
    } yield {
      List(a(x), b(x))
    }).toList
  }

  //find sum and multiplication of the list (dont use inbuilt methods)
  def sumMultiply(): (Int, Int) = {
    val lst1 = List(1, 2, 3, 4)
    val sum = lst1.foldLeft(0)(_ + _)
    val multiply = lst1.foldLeft(1)(_ * _)
    (sum, multiply)
  }

  //Implement Stack and Queue using Lists.

  def stackQueue() = {
    val lst1 = List(1, 2, 3, 4)
    val stackList = lst1 :+ 3
    val queueLst = 5 :: lst1
  }
}

object ForOps extends App {
  val op = new Ops
}
