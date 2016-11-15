package com.gmail.pderichai.text.classification
import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.DenseVector
import ch.ethz.dal.tinyir.io.ReutersRCVStream

// Class to use an SVM to classify documents
object SVMMain {
  val NUM_TERMS = 3000
  val LAMBDA = 0.001
  val NUM_CODES = 300

  def docs = new ReutersRCVStream("src/main/resources/train").stream
  val numDocs = docs.size
  // NUMDOCS number of terms in the dictionary
  val topTerms = Utils.getTopTerms(docs, NUM_TERMS)
  // Map: term -> index in vectors
  val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
  // Map: doc -> most common terms in doc that are in topTerms
  val docTermFreqs = docs.map(doc => doc -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap
  // Map: (codes, index) -> feature vector of doc with codes
  val codesToFeatureVectors = docTermFreqs.zipWithIndex.map { case ((t), index) => ((t._1.codes, index), Utils.getFeatureVector(t._2, termToIndexInFeatureVector, NUM_TERMS))}.toSeq

  // training thetas
  val thetas = Utils.getTopCodes(docs, 300).map(code => (code, SVM.getTheta(code, codesToFeatureVectors, NUM_TERMS)))


  def runOnValidationSet() : Unit = {
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
        // populate feature vector with term frequencies of document
        docTermFreq.foreach { case (term, freq) => if (termToIndexInFeatureVector.contains(term)) featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble }

        val prediction = theta.dot(featureVector)

        if (prediction > 0) {
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
        runningF1 += f1
      }
    }
    println("overall f1: " + runningF1 / numDocs)
  }


  def runOnTestSet() : Unit = {
    val testDocs = new ReutersRCVStream("src/main/resources/test").stream

    val file = new File("ir-2016-project-13-svm.txt")
    val writer = new BufferedWriter(new FileWriter(file))

    for (doc <- testDocs) {
      writer.write(doc.ID.toString)

      for ((code, theta) <- thetas) {
        val docTermFreq = Utils.getTermFrequencies(doc)
        val featureVector = DenseVector.zeros[Double](NUM_TERMS)
        // populate feature vector with term frequencies of document
        docTermFreq.foreach { case (term, freq) => if (termToIndexInFeatureVector.contains(term)) featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble }

        val prediction = theta.dot(featureVector)

        if (prediction > 0) {
          writer.write(" " + code)
        }
      }
      writer.write("\n")
    }
  }
}