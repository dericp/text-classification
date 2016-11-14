package com.gmail.pderichai.text.classification

import breeze.linalg.{DenseVector, Vector}

/**
  * TODO: Using SparseVectors right now since it seems to give better performance.
  */
object SVM {

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
