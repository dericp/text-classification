package edu.washington.cs.dericp.text.classification

import breeze.linalg.{DenseVector, Vector}

// class with util functions of an SVM
object SVM {

  case class DataPoint(x: Vector[Double], y:Double)

  // Takes theta, a data point with (feature vector, y classification), lambda, step
  // Returns the updated theta value
  def updateStep(theta: DenseVector[Double], p: DataPoint, lambda: Double, step: Int) = {
    val thetaShrink = theta * (1 - 1.0/step.toDouble)
    val margin = 1.0 - (p.y * theta.dot(p.x))
    if (margin <= 0)
      thetaShrink
    else
      thetaShrink + (p.x * (1.0 / (lambda * step)) * p.y)
  }

  // Trains and returns theta for one code
  def getTheta(code: String, codesToFeatureVectors: Seq[((Set[String], Int), DenseVector[Double])], numUniqueTerms: Int): DenseVector[Double] = {
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1
    val lambda = 0.001

    for ((t, featureVector) <- codesToFeatureVectors) {
      val codes = t._1
      theta = updateStep(theta, new DataPoint(featureVector, if (codes.contains(code)) 1.0 else 0.0), lambda, timeStep)
      timeStep += 1
    }

    theta
  }
}
