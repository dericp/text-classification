package com.gmail.pderichai.text.classification

import java.io.{BufferedWriter, File, FileWriter}

import breeze.linalg.{DenseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

import scala.collection.mutable.ListBuffer

object SVMMain {

  def main(args: Array[String]): Unit = {
    println("starting")

    val NUM_TERMS = 1000
    val LAMBDA = 0.001

    val docs = new ReutersRCVStream("src/main/resources/train").stream

    // Pruning dictionary to NUM_TERMS terms
    val topTerms = Utils.getTopTerms(docs, NUM_TERMS)
    // Map: term -> index
    val termToIndexInFeatureVector = topTerms.zipWithIndex.toMap
    // Map: doc ID -> term frequencies of document
    val docTermFreqs = docs.map(doc => doc.ID -> Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))).toMap
    // Map: doc ID -> document feature vector
    val docFeatureVectors = docTermFreqs.map { case (docID, termFreq) => (docID, Utils.getFeatureVector(termFreq, DenseVector.zeros[Double](NUM_TERMS), termToIndexInFeatureVector)) }
    def emptyTheta = DenseVector.zeros[Double](NUM_TERMS)
    // Map: category -> empty vector (initial theta)
    val thetas = Utils.getCodes.map{case(code) => code -> emptyTheta}.toMap
    var timeStep = 1

    println("setup finished")

    // training
    for (i <- util.Random.shuffle(0 to docs.size - 1)) {
      val doc = docs(i)
      val docTermFreq = docTermFreqs(doc.ID)
      val featureVector = docFeatureVectors(doc.ID)
      for ((code, theta) <- thetas) {
        val y = if (doc.codes.contains(code)) 1 else -1
        var theta = thetas(code)
        theta = SVM.updateStep(theta, new SVM.DataPoint(featureVector, y), LAMBDA, timeStep)
        for (i <- (0 to theta.length - 1)) {
          thetas(code)(i) = theta(i)
        }
        timeStep += 1
      }
    }

    println("training finished")

    // validation
    val validationDocs = new ReutersRCVStream("src/main/resources/validation").stream
    val f1Scores = ListBuffer.empty[Double]

    for (doc <- validationDocs) {
      val termFreq = Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))
      val featureVector = Utils.getFeatureVector(termFreq, DenseVector.zeros[Double](NUM_TERMS), termToIndexInFeatureVector)
      val assignedCats = collection.mutable.Set.empty[String]
      val correctCats = doc.codes

      for ((code, theta) <- thetas) {
        val lossNotInCat = SVM.hingeLoss(theta, featureVector, -1)
        val lossInCat = SVM.hingeLoss(theta, featureVector, 1)
        if (lossInCat < lossNotInCat) assignedCats.+=(code)
      }
      f1Scores += docF1Score(assignedCats, correctCats)
    }

    val avgF1Score = f1Scores.sum / f1Scores.size
    println("f1 score: " + avgF1Score)

    // test
//    val testDocs = new ReutersRCVStream("src/main/resources/test").stream
//    val file = new File("ir-2016-project-13-svm.txt")
//    val bw = new BufferedWriter(new FileWriter(file))
//
//    for (doc <- testDocs) {
//      bw.write(doc.ID.toString)
//      val termFreq = Utils.getTermFrequencies(doc).filter(t => topTerms.contains(t._1))
//      val featureVector = Utils.getFeatureVector(termFreq, DenseVector.zeros[Double](NUM_TERMS), termToIndexInFeatureVector)
//      val assignedCats = collection.mutable.Set.empty[String]
//      val correctCats = doc.codes
//
//      for ((code, theta) <- thetas) {
//        val lossNotInCat = SVM.hingeLoss(theta, featureVector, -1)
//        val lossInCat = SVM.hingeLoss(theta, featureVector, 1)
//        if (lossInCat < lossNotInCat) bw.write(" " + code)
//      }
//      bw.write("\n")
//    }
//    bw.close()
  }

  // Returns F1 score for doc = 2PR / (P + R)
  def docF1Score(foundCats: scala.collection.Set[String], correctCats: Set[String]): Double = {
    val relevantRetrieved = foundCats.count(correctCats(_)).toDouble
    if (relevantRetrieved == 0) return 0
    val p = relevantRetrieved / foundCats.size
    val r = relevantRetrieved / correctCats.size
    2 * p * r / (p + r)
  }
}