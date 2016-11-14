package com.gmail.pderichai.text.classification

import breeze.linalg.{DenseVector, SparseVector, Vector}
import ch.ethz.dal.tinyir.io.ReutersRCVStream

object SVMMain {

  def main(args: Array[String]): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream

    val termToIndexInFeatureVector = docs.flatMap(_.tokens).distinct.zipWithIndex.toMap
    // temp hard-coded code
    val code = "USA"
    var timeStep = 1
    // 178069 is the number of unique words in the training vocabulary
    var theta = DenseVector.zeros[Double](178069)
    val lambda = 0.001

    // random testing code
    //val docVec = SparseVector.zeros[Double](termToIndexInFeatureVector.size)
    //shortenContent(docs(0)).termFreq.foreach{case(term, freq) => docVec(termToIndexInFeatureVector.get(term).get) = freq.toDouble}

    for (i <- util.Random.shuffle(0 to docs.size - 1)) {
      val doc = docs(i)
      val docTermFreq = Utils.getTermFrequencies(doc)
      val featureVector = Vector.zeros[Double](termToIndexInFeatureVector.size)
      val y = if(doc.codes.contains(code)) 1 else -1

      // populating the featureVector with term frequencies from the current document
      docTermFreq.foreach{case(term, freq) => featureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble}

      theta = SVM.updateStep(theta, new SVM.DataPoint(featureVector, y), lambda, timeStep)
      timeStep += 1

      //println("step: " + timeStep)
    }
    //println(theta.dot(docVec))
  }
}