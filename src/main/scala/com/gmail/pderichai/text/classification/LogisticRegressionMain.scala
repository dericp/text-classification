package com.gmail.pderichai.text.classification
import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object LogisticRegressionMain {
  val docs = new ReutersRCVStream("src/main/resources/train").stream

  val numDocs = docs.size
  val numTerms = 3000

  val topTerms = Utils.getTopTerms(docs, numTerms)
  val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
  val docTermFreqs = docs.map(doc => doc -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap

  val codesToFeatureVectors = docTermFreqs.zipWithIndex.map { case ((t), index) => ((t._1.codes, index), Utils.getFeatureVector(t._2, termToIndexInFeatureVector, numTerms))}.toSeq

  // all the training is in this step
  val thetas = Utils.getTopCodes(docs, 600).map(code => (code, LogisticRegression.getTheta(code, codesToFeatureVectors, numTerms, numDocs, docs.filter(_.codes.contains(code)).size)))


  def runOnValidationSet() : Unit = {
    val validationDocs = new ReutersRCVStream("src/main/resources/validation").stream

    var runningF1 = 0.0

    for (doc <- validationDocs) {
      var TP = 0.0
      var FP = 0.0
      var TN = 0.0
      var FN = 0.0

      for ((code, theta) <- thetas) {
        val docTermFreq = docTermFreqs(doc)
        val featureVector = Utils.getFeatureVector(docTermFreq, termToIndexInFeatureVector, numTerms)

        val prediction = LogisticRegression.logistic(theta, featureVector)

        if (prediction > 0.8) {
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
      runningF1 += (2 * precision * recall) / (precision + recall)
    }
    println("overall f1: " + runningF1 / numDocs)
  }

  def runOnTestSet(): Unit = {
    val testDocs = new ReutersRCVStream("src/main/resources/test").stream

    val file = new File("ir-2016-project-13-lr.txt")
    val writer = new BufferedWriter(new FileWriter(file))

    for (doc <- testDocs) {
      writer.write(doc.ID.toString)

      for ((code, theta) <- thetas) {
        val docTermFreq = Utils.getTermFrequencies(doc)
        val featureVector = Utils.getFeatureVector(docTermFreq, termToIndexInFeatureVector, numTerms)

        val prediction = LogisticRegression.logistic(theta, featureVector)

        if (prediction > 0.8) {
          writer.write(" " + code)
        }
      }
      writer.write("\n")
    }
    writer.close()
  }
}