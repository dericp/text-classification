package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object LogisticRegressionMain {
  val NUM_TERMS = 3000
  val NUM_CODES = 6000

  def main(args: Array[String]): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream
    val numDocs = docs.size

    // Precompute dictionary, Map: term -> index, feature vectors
    val topTerms = Utils.getTopTerms(docs, NUM_TERMS)
    val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
    val docTermFreqs = docs.map(doc => doc -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap
    val codesToFeatureVectors = docTermFreqs.zipWithIndex.map { case ((t), index) => ((t._1.codes, index), Utils.getFeatureVector(t._2, termToIndexInFeatureVector, NUM_TERMS))}.toSeq

    // training thetas
    val thetas = Utils.getTopCodes(docs, NUM_CODES).map(code => (code, LogisticRegression.getTheta(code, codesToFeatureVectors, NUM_TERMS, numDocs, docs.filter(_.codes.contains(code)).size)))

    // validation
    val validationDocs = new ReutersRCVStream("src/main/resources/validation").stream
    var runningF1 = 0.0

    for (doc <- validationDocs) {
      var TP = 0.0
      var FP = 0.0
      var TN = 0.0
      var FN = 0.0

      for ((code, theta) <- thetas) {
        val docTermFreq = Utils.getTermFrequencies(doc)
        val featureVector = DenseVector.zeros[Double](NUM_TERMS)
        docTermFreq.foreach { case (term, freq) => if (termToIndexInFeatureVector.contains(term)) (featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble) }

        val prediction = LogisticRegression.logistic(theta, featureVector)

        if (prediction > 0.5) {
          if (doc.codes.contains(code)) {
            TP = TP + 1
          } else {
            FP = FP + 1
          }
        } else {
          if (doc.codes.contains(code)) {
            FN = FN + 1
          } else {
            TN = TN + 1
          }
        }
      }

      val precision = TP / (TP + FP)
      val recall = TP / (TP + FN)
      val f1 = (2 * precision * recall) / (precision + recall)
      if (f1.isNaN) {
        runningF1 += 0
      } else {
        runningF1 += (2 * precision * recall) / (precision + recall)
      }
    }
    println("overall f1: " + runningF1 / numDocs)
  }
}
