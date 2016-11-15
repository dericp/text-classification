package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector}
import ch.ethz.dal.tinyir.processing.XMLDocument

object LogisticRegression {

  def newTheta(theta: DenseVector[Double], featureVector: DenseVector[Double], docIsInCategory: Boolean, timeStep: Int, alphaPlus: Int, alphaMinus: Int): DenseVector[Double] = {
    val z = if (docIsInCategory) alphaMinus * (1 - logistic(theta, featureVector)) else -alphaPlus * (logistic(theta,featureVector))
    theta + ((featureVector) * (z * 1.0 / timeStep))
  }

  def logistic(theta: DenseVector[Double], featureVector: DenseVector[Double]) : Double = {
    1.0 / (1.0 + Math.exp(-(featureVector.dot(theta))))
  }

  def getTheta(code: String, codesToFeatureVectors: Seq[((Set[String], Int), DenseVector[Double])], numUniqueTerms: Int, numDocs: Int): DenseVector[Double] = {
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1
    val alphaPlus = 1
    //println(alphaPlus)
    val alphaMinus = numDocs - alphaPlus

    for ((t, featureVector) <- codesToFeatureVectors) {
      //("time step: " + timeStep)
      //println("feature vector: " + featureVector.findAll(_ > 0))

      theta = LogisticRegression.newTheta(theta, featureVector, t._1.contains(code), timeStep, alphaPlus, alphaMinus)
      timeStep += 1
    }

    println("got a theta for code: " + code)

    theta
  }
}
