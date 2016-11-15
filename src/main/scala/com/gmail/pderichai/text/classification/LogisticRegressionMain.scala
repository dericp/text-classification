package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object LogisticRegressionMain {

  def main(args: Array[String]): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream

    val numDocs = docs.size
    val numTerms = 1000

    val topTerms = Utils.getTopTerms(docs, numTerms)
    val docTermFreqs = docs.map(doc => doc.ID -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap
    val alphaPluses = Utils.getCodes.map(code => code -> docs.filter(_.codes.contains(code)).size).toMap
    val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
    val docFeatureVectors = docTermFreqs.map { case (docID, termFreq) => (docID, Utils.getFeatureVector(termFreq, DenseVector.zeros[Double](numTerms), termToIndexInFeatureVector)) }

    val thetas = Utils.getCodes().map(code => (code, LogisticRegression.getTheta(docs, numDocs, code, termToIndexInFeatureVector, docFeatureVectors, numTerms, alphaPluses)))

    // EVERYTHING BEYOND HERE IS VALIDATION
    val validationDocs = new ReutersRCVStream("src/main/resources/validation").stream

    for (doc <- validationDocs) {
      var TP = 0.0
      var FP = 0.0
      var TN = 0.0
      var FN = 0.0

      for ((code, theta) <- thetas) {
        val docTermFreq = Utils.getTermFrequencies(doc)
        val featureVector = DenseVector.zeros[Double](numTerms)
        docTermFreq.foreach { case (term, freq) => if (termToIndexInFeatureVector.contains(term)) (featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble) }

        val prediction = LogisticRegression.logistic(theta, featureVector)

        //println("prediction: " + prediction + " correct " + doc.codes.contains(code))

        if (prediction > 0.5) {
          println("predicted doc was in: " + code)
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
      println("doc was actually in: " + doc.codes)

      val precision = TP / (TP + FP)
      val recall = TP / (TP + FN)
      println("precision: " + (TP / (TP + FP)))
      println("recall: " + (TP / (TP + FN)))
      println("F1: " + ((2 * precision * recall) / (precision + recall)))
    }
  }
}
