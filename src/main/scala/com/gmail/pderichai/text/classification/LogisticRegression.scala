package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.processing.XMLDocument

object LogisticRegression {

  def newTheta(theta: Map[Int, Double], featureVector: Map[Int, Int], termToIndexInFeatureVector: Map[String, Int], docIsInCategory: Boolean, timeStep: Int, alphaPlus: Int, alphaMinus: Int): Map[Int, Double] = {
    val z = if (docIsInCategory) alphaMinus * (1 - logistic(theta, featureVector)) else -alphaPlus * (logistic(theta,featureVector))
    theta.map{case(index, weight) => (index, weight + featureVector(index) * z)}
    //theta + ((featureVector.mapValues(_ * z)))
    //theta + ((featureVector * z) * (1.0 / Math.sqrt(timeStep)))
  }

  def logistic(theta: Map[Int, Double], featureVector: Map[Int, Int]) : Double = {
    1.0 / (1.0 + Math.exp(-1 * theta.map{case(index, weight) => (index, (featureVector(index) * weight))}.keys.sum))
    //theta.map{case(index, weight) => (index, 1.0 / (1.0 + Math.exp(-1 * featureVector(index) * weight)))}
    //1.0 / (1.0 + Math.exp(-1 * (featureVector.dot(theta))))
  }

  def getTheta(docs: Seq[XMLDocument], code: String, termToIndexInFeatureVector: Map[String, Int], numUniqueTerms: Int): Map[Int, Double] = {
    var theta = (0 to numUniqueTerms - 1) map (index => index -> 0.0) toMap
    var timeStep = 1;
    val alphaPlus = docs.filter(_.codes.contains((code))).size
    val alphaMinus = 50000 - alphaPlus

    for (i <- util.Random.shuffle(0 to docs.size - 1)) {
      //println("time step: " + timeStep)
      val doc = docs(i)
      val docTermFreq = Utils.getTermFrequencies(doc)
      val featureVector = docTermFreq.map{case(term, freq) => (termToIndexInFeatureVector(term), freq)}
      //docTermFreq.foreach { case (term, freq) => featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble }

      //println(featureVector.findAll(_ > 0))

      theta = LogisticRegression.newTheta(theta, featureVector, termToIndexInFeatureVector, doc.codes.contains(code), timeStep, alphaPlus, alphaMinus)
      timeStep += 1
    }
    println("got a theta for code: " + code)
    theta
  }
}
