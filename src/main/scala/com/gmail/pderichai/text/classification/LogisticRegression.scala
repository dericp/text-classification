package com.gmail.pderichai.text.classification
import breeze.linalg
import breeze.linalg.SparseVector

/**
  * Created by dericp on 11/14/16.
  */

//    val vec = breeze.linalg.SparseVector.zeros[Integer](5)
//    vec(2) = 1
//    vec(2) += 2
//    println("VECTOR: " + vec)

object LogisticRegression {
  // Returns value that we have to subtract from theta to get new theta
  def updateTheta(thetaVec: SparseVector[Double], docVec: SparseVector[Double], docIsInCategory: Boolean, step: Int): SparseVector[Double] = {
    val z = if (docIsInCategory) (1-logistic(thetaVec,docVec)) else (-logistic(thetaVec,docVec))
    //println("doc term freqs: " + docVec)
    //println("update: " + z)
    //println("subtract: " + (docVec * z))
    //docSMap.termFreqs.keySet.filter(!theta.termFreqs.keySet.contains(_))
    //docSMap.termFreqs.foreach{ case (k, v) => theta.termFreqs(k) = theta.termFreqs.getOrElse(k, 0.0) - v}
    thetaVec - ((docVec * z) * (1.0 / step))
  }

  def logistic(docVec: SparseVector[Double], thetaVec: SparseVector[Double]) : Double =
  // 1.0 / (1.0 + Math.exp(-1 * (docVec.dot(thetaVec)) - b))
    1.0 / (1.0 + Math.exp(-1 * (docVec.dot(thetaVec))))
}
