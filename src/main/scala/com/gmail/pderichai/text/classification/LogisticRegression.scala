package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.processing.XMLDocument

object LogisticRegression {

<<<<<<< HEAD
  def newTheta(theta: DenseVector[Double], featureVector: DenseVector[Double], docIsInCategory: Boolean, timeStep: Int, alphaPlus: Int, alphaMinus: Int): DenseVector[Double] = {
    val z = if (docIsInCategory) alphaMinus * (1 - logistic(theta, featureVector)) else -alphaPlus * (logistic(theta,featureVector))
    theta + ((featureVector * z) * (1.0 / timeStep))
  }

  def logistic(theta: DenseVector[Double], featureVector: DenseVector[Double]) : Double = {
    1.0 / (1.0 + Math.exp(-1 * (featureVector.dot(theta))))
  }

  def getTheta(docs: Seq[XMLDocument], code: String, termToIndexInFeatureVector: Map[String, Int], docFeatureVectors: Map[Int, DenseVector[Double]], numUniqueTerms: Int, alphaPluses: Map[String, Int]): DenseVector[Double] = {
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1
    val alphaPlus = alphaPluses(code)
    val alphaMinus = 50000 - alphaPlus

    for (i <- util.Random.shuffle(0 to 49999)) {
      //println("time step: " + timeStep)
      val doc = docs(i)
      val featureVector = docFeatureVectors(doc.ID)
      //val featureVector = DenseVector.zeros[Double](numUniqueTerms)
      //val docTermFreq = docTermFreqs(doc.ID)
      //val docTermFreq = Utils.getTermFrequencies(doc).toList.sortWith(_._2 > _._2).take(40).toMap
      //docTermFreq.foreach { case (term, freq) => featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble }

      //println(featureVector.findAll(_ > 0))

      theta = LogisticRegression.newTheta(theta, featureVector, doc.codes.contains(code), timeStep, alphaPlus, alphaMinus)
      timeStep += 1
    }
    println("got a theta for code: " + code)
    theta
  }
}
