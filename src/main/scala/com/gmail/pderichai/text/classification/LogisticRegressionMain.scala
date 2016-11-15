package com.gmail.pderichai.text.classification
import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object LogisticRegressionMain {

  def main(args: Array[String]): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream

    val termToIndexInFeatureVector = docs.flatMap(_.tokens).distinct.zipWithIndex.toMap
    val numUniqueTerms = termToIndexInFeatureVector.size

    val thetas = Utils.getCodes().map(code => (code, LogisticRegression.getTheta(docs, code, termToIndexInFeatureVector, numUniqueTerms)))

    val validationDocs = new ReutersRCVStream("src/main/resources/validation").stream

    var TP = 0.0
    var FP = 0.0
    var TN = 0.0
    var FN = 0.0

    for (doc <- validationDocs) {
      val docTermFreq = Utils.getTermFrequencies(doc)
      val featureVector = SparseVector.zeros[Double](numUniqueTerms)
      docTermFreq.foreach{case(term, freq) => if (termToIndexInFeatureVector.contains(term)) (featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble)}

      val prediction = LogisticRegression.logistic(theta, featureVector)

      println("prediction: " + prediction + " correct " + doc.codes.contains(code))

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
    println("precision: " + (TP / (TP + FP)))
    println("recall: " + (TP / (TP + FN)))
    println("F1: " + ((2 * precision * recall) / (precision + recall)))
  }
}
