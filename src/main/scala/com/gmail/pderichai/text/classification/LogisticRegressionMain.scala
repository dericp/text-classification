package com.gmail.pderichai.text.classification
import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object LogisticRegressionMain {

  def main(args: Array[String]): Unit = {
    //runOnTestSet()
    val docs = new ReutersRCVStream("src/main/resources/train").stream

    val numDocs = docs.size
    println(numDocs)
    val numTerms = 100

    val topTerms = Utils.getTopTerms(docs, numTerms)
    val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
    //println(termToIndexInFeatureVector)
    // this line is a little questionable
    val docTermFreqs = docs.map(doc => doc -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap
    println(docTermFreqs.size)

    val codesToFeatureVectors = docTermFreqs.zipWithIndex.map { case ((t), index) => ((t._1.codes, index), Utils.getFeatureVector(t._2, termToIndexInFeatureVector, numTerms))}.toSeq
    println(codesToFeatureVectors.size)
    //println(docTermFreqs)
    //val alphaPluses = Utils.getCodes.map(code => code -> docs.filter(_.codes.contains(code)).size).toMap
    //val codesToFeatureVectors = docTermFreqs.map { case (doc, termFreq) => (doc.codes, Utils.getFeatureVector(termFreq, termToIndexInFeatureVector)) }.toSeq
    //println(codesToFeatureVectors.size)

    // all the training is in this step
    val thetas = Utils.getCodes().map(code => (code, LogisticRegression.getTheta(code, codesToFeatureVectors, numTerms, numDocs)))

    // EVERYTHING BEYOND HERE IS VALIDATION
    val validationDocs = new ReutersRCVStream("src/main/resources/validation").stream

    var runningF1 = 0.0

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
          //println("predicted doc was in: " + code)
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
      //println("doc was actually in: " + doc.codes)

      val precision = TP / (TP + FP)
      val recall = TP / (TP + FN)
      println("precision: " + (TP / (TP + FP)))
      println("recall: " + (TP / (TP + FN)))
      println("F1: " + ((2 * precision * recall) / (precision + recall)))
      runningF1 += (2 * precision * recall) / (precision + recall)
      println("overall f1: " + runningF1 / numDocs)
    } 
  }

  def runOnTestSet(): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream.take(10000)

    val numDocs = docs.size
    val numTerms = 1000

    val topTerms = Utils.getTopTerms(docs, numTerms)
    val docTermFreqs = docs.map(doc => doc.ID -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap
    val alphaPluses = Utils.getCodes.map(code => code -> docs.filter(_.codes.contains(code)).size).toMap
    val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
    val docFeatureVectors = docTermFreqs.map { case (docID, termFreq) => (docID, Utils.getFeatureVector(termFreq, DenseVector.zeros[Double](numTerms), termToIndexInFeatureVector)) }

    val thetas = Utils.getCodes().map(code => (code, LogisticRegression.getTheta(docs, numDocs, code, termToIndexInFeatureVector, docFeatureVectors, numTerms, alphaPluses)))



    val file = new File("ir-2016-project-13-lr.txt")
    val bw = new BufferedWriter(new FileWriter(file))

    val testDocs = new ReutersRCVStream("src/main/resources/test").stream

    for (doc <- testDocs) {
      var TP = 0.0
      var FP = 0.0
      var TN = 0.0
      var FN = 0.0

      bw.write(doc.ID.toString)

      for ((code, theta) <- thetas) {

        val docTermFreq = Utils.getTermFrequencies(doc)
        val featureVector = DenseVector.zeros[Double](numTerms)
        docTermFreq.foreach { case (term, freq) => if (termToIndexInFeatureVector.contains(term)) (featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble) }

        val prediction = LogisticRegression.logistic(theta, featureVector)

        //println("prediction: " + prediction + " correct " + doc.codes.contains(code))

        if (prediction > 0.5) {
          bw.write(" " + code)

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
      bw.write("\n")
    }

    bw.close()
  }
}
