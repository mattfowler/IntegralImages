package org.codingwithclarity

import org.codingwithclarity.IntegralImageCalculator._
import org.scalatest.{FlatSpec, Matchers}


class IntegralImageCalculatorTest extends FlatSpec with Matchers {

  it should "be able to sum a one row matrix" in {
    val input = List(List(1, 1, 0, 0, 0, 1, 1))

    imageMethods.foreach(imageFunction => {
      val integralImg = imageFunction(input)
      integralImg.head should be(List(1, 2, 2, 2, 2, 3, 4))
    })
  }

  it should "be able to sum multi row matrices" in {
    val input = List(List(1, 1, 0, 0, 0, 1, 1),
      List(0, 0, 1, 0, 1, 1, 1))

    imageMethods.foreach(imageFunction => {
      val integralImg = imageFunction(input)
      integralImg.head should be(List(1, 2, 2, 2, 2, 3, 4))
      integralImg.last should be(List(1, 2, 3, 3, 4, 6, 8))
    })

    val threeRows = List(List(1, 1, 0, 0, 1, 1),
      List(0, 0, 1, 1, 0, 0),
      List(1, 0, 0, 0, 0, 1))

    imageMethods.foreach(imageFunction => {
      val integralImg = imageFunction(threeRows)
      integralImg.head should be(List(1, 2, 2, 2, 3, 4))
      integralImg.last should be(List(2, 3, 4, 5, 6, 8))
    })
  }
}
