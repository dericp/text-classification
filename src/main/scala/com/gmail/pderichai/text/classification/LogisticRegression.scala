package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.processing.XMLDocument

object LogisticRegression {

  def newTheta(theta: DenseVector[Double], featureVector: SparseVector[Double], docIsInCategory: Boolean, timeStep: Int, alphaPlus: Int, alphaMinus: Int): DenseVector[Double] = {
    val z = if (docIsInCategory) alphaMinus * (1 - logistic(theta, featureVector)) else -alphaPlus * (logistic(theta,featureVector))
    theta + ((featureVector * z) * (1.0 / Math.sqrt(timeStep)))
  }

  def logistic(theta: DenseVector[Double], featureVector: SparseVector[Double]) : Double = {
    1.0 / (1.0 + Math.exp(-1 * (featureVector.dot(theta))))
  }

  def getTheta(docs: Seq[XMLDocument], code: String, termToIndexInFeatureVector: Map[String, Int], numUniqueTerms: Int): DenseVector[Double] = {
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1;
    val alphaPlus = docs.filter(_.codes.contains((code))).size
    val alphaMinus = 50000 - alphaPlus

    for (i <- util.Random.shuffle(0 to docs.size - 1)) {
      println("time step: " + timeStep)
      val doc = docs(i)
      val featureVector = SparseVector.zeros[Double](numUniqueTerms)
      val docTermFreq = Utils.getTermFrequencies(doc)
      docTermFreq.foreach { case (term, freq) => featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble }

      //println(featureVector.findAll(_ > 0))

      theta = LogisticRegression.newTheta(theta, featureVector, doc.codes.contains(code), timeStep, alphaPlus, alphaMinus)
      timeStep += 1
    }
    theta
  }
}
