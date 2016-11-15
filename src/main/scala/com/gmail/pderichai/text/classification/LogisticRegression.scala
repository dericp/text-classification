package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}

object LogisticRegression {

  def newTheta(theta: DenseVector[Double], featureVector: SparseVector[Double], docIsInCategory: Boolean, timeStep: Int, alphaPlus: Int, alphaMinus: Int): DenseVector[Double] = {
    val z = if (docIsInCategory) alphaMinus * (1 - logistic(theta, featureVector)) else -alphaPlus * (logistic(theta,featureVector))
    theta + ((featureVector * z) * (1.0 / Math.sqrt(timeStep)))
  }

  def logistic(theta: DenseVector[Double], featureVector: SparseVector[Double]) : Double = {
    1.0 / (1.0 + Math.exp(-1 * (featureVector.dot(theta))))
  }
}
