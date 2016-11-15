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

  def getTheta(docs: Seq[XMLDocument], numDocs: Int, code: String, termToIndexInFeatureVector: Map[String, Int], docFeatureVectors: Map[Int, DenseVector[Double]], numUniqueTerms: Int, alphaPluses: Map[String, Int]): DenseVector[Double] = {
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1
    val alphaPlus = alphaPluses(code)
    val alphaMinus = numDocs - alphaPlus

    for (i <- util.Random.shuffle(0 to numDocs - 1)) {
      println("time step: " + timeStep)

      val doc = docs(i)
      val featureVector = docFeatureVectors(doc.ID)

      //println("feature vector: " + featureVector.findAll(_ > 0))

      theta = LogisticRegression.newTheta(theta, featureVector, doc.codes.contains(code), timeStep, alphaPlus, alphaMinus)
      timeStep += 1
    }

    println("got a theta for code: " + code)

    theta
  }
}
