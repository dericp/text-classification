package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object LogisticRegressionMain {

  def main(args: Array[String]): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream

    val termToIndexInFeatureVector = docs.flatMap(_.tokens).distinct.zipWithIndex.toMap
    val numUniqueTerms = termToIndexInFeatureVector.size
    var theta = DenseVector.zeros[Double](numUniqueTerms)
    var timeStep = 1;

    val code = "USA"

    for (i <- util.Random.shuffle(0 to docs.size - 1)) {
      println("time step: " + timeStep)
      val doc = docs(i)
      val featureVector = SparseVector.zeros[Double](numUniqueTerms)
      val docTermFreq = Utils.getTermFrequencies(doc)
      docTermFreq.foreach{case(term, freq) => featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble}


      // hard-coded code for testing
      theta = LogisticRegression.updateTheta(theta, featureVector, doc.codes.contains(code), timeStep)
      timeStep += 1
    }

    // test objects
    val testFeatureVector = SparseVector.zeros[Double](numUniqueTerms)
    val testDocTF = Utils.getTermFrequencies(docs(0))
    testDocTF.foreach{case(term, freq) => testFeatureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble}

    println(LogisticRegression.logistic(theta, testFeatureVector))
  }
}
