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

  def getTheta(code: String, codesToFeatureVectors: Seq[((Set[String], Int), DenseVector[Double])], numUniqueTerms: Int): DenseVector[Double] = {
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1
    val lambda = 0.001
    //println(alphaPlus)

    for ((t, featureVector) <- codesToFeatureVectors) {
      //("time step: " + timeStep)
      //println("feature vector: " + featureVector.findAll(_ > 0))
      val codes = t._1

      theta = updateStep(theta, new DataPoint(featureVector, if (codes.contains(code)) 1.0 else 0.0), lambda, timeStep)
      timeStep += 1
    }

    println("got a theta for code: " + code)

    theta
  }
}
