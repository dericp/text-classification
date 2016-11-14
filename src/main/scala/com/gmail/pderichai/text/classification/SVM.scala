package com.gmail.pderichai.text.classification

import breeze.linalg.{Vector, DenseVector}


/**
  * Created by Isa on 11/13/2016.
  */
object SVM {

  /* just the slide code */
  case class DataPoint(x: Vector[Double], y:Double)

  def updateStep(theta: DenseVector[Double], p: DataPoint, lambda: Double, step:Int) = {
    val thetaShrink = theta * (1 - 1.0/step.toDouble)
    val margin = 1.0 - (p.y * theta.dot(p.x))
    if (margin <= 0)
      thetaShrink
    else
      thetaShrink + (p.x * (1.0 / (lambda * step)) * p.y)
  }

}
