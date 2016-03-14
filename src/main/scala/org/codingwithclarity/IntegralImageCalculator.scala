package org.codingwithclarity

import scala.annotation.tailrec
import scala.collection.mutable.MutableList

object IntegralImageCalculator {

  type Matrix[T] = List[List[T]]

  val imageMethods:List[(Matrix[Int] => Matrix[Int])] = List(integralImageScanLeft,
                                                             integralImageTranspose,
                                                             integralImageRecursive,
                                                             integralImageIterative)

  def integralImageIterative(image: Matrix[Int]) = {
    def cumulativeSum(row: List[Int]) = {
      val rowResult: MutableList[Int] = MutableList.fill(row.length)(0)
      var currentSum = 0
      for (i <- row.indices) {
        currentSum = currentSum + row(i)
        rowResult(i) = currentSum
      }
      rowResult
    }
    val result = MutableList.fill(image.length)(MutableList.fill(image.head.length)(0))
    var previousRow = MutableList.fill(image.head.length)(0)
    for (row <- image.indices) {
      val rowCumSum = cumulativeSum(image(row))
      for (column <- image(row).indices) {
        result(row)(column) = rowCumSum(column) + previousRow(column)
      }
      previousRow = result(row)
    }
    result.toList.map(_.toList)
  }

  def integralImageScanLeft(image: Matrix[Int]): Matrix[Int] = {
    val zeroes: List[Int] = List.fill[Int](image.head.length)(0)
    val cumSum = image.map(row => row.scanLeft(0)(_ + _).tail)
    cumSum.scanLeft(zeroes)((priorRow, currentRow) =>
                            (priorRow, currentRow).zipped.map(_ + _)).tail
  }

  def integralImageTranspose(image: Matrix[Int]): Matrix[Int] = {
    def cumulativeSum(row: List[Int]) = row.scanLeft(0)(_ + _).tail
    image.map(cumulativeSum).transpose.map(cumulativeSum).transpose
  }

  @tailrec
  private def cumulativeSum(numbers: List[Int], accum: List[Int] = List(0)): List[Int] = {
    numbers match {
      case Nil => accum.tail
      case (head :: tail) => cumulativeSum(tail,  accum :+ (head + accum.last))
    }
  }

  def integralImageRecursive(numbers: Matrix[Int]) = {
    @tailrec
    def helper(numbers: Matrix[Int], result: Matrix[Int]): Matrix[Int] = {
      numbers match {
        case Nil => result.tail
        case head :: tail =>
          val summedRow = (cumulativeSum(head), result.last).zipped.map(_ + _)
          helper(tail, result :+ summedRow)
      }
    }
    helper(numbers, List(List.fill[Int](numbers.head.length)(0)))
  }

}
