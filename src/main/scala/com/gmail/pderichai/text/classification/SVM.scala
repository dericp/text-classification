package com.gmail.pderichai.text.classification

import breeze.linalg.{DenseVector, Vector}

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

  def hingeLoss(theta: DenseVector[Double], docVector: DenseVector[Double], y: Int): Double = {
    Math.max(-theta.dot(docVector) * y + 1, 0)
  }

  def risk(theta: DenseVector[Double], lambda: Double, docIDToCats: Map[Int, Set[String]], docIDToVec: Map[Int, DenseVector[Double]], trainingSetSize: Int, cat: String): Double = {
    val docIDToY = docIDToCats.map{case(id, cats) => (id, cats.contains(cat))}.mapValues(if(_) 1 else -1)
    lambda / 2 * theta.dot(theta) + 1.0 / trainingSetSize * docIDToY.keySet.map{case(id) => hingeLoss(theta, docIDToVec(id), 1)}.sum
  }
}
