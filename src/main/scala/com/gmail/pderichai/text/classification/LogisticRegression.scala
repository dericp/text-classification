package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}

object LogisticRegression {

  // Returns value that we have to subtract from theta to get new theta
  def updateTheta(theta: DenseVector[Double], featureVector: SparseVector[Double], docIsInCategory: Boolean, step: Int): DenseVector[Double] = {
    val z = if (docIsInCategory) (1 - logistic(theta, featureVector)) else (-logistic(theta,featureVector))
    theta - ((featureVector * z) * (1.0 / step))
  }

  def logistic(theta: DenseVector[Double], featureVector: SparseVector[Double]) : Double = {
    1.0 / (1.0 + Math.exp(-1 * (featureVector.dot(theta))))
  }
}
