package com.gmail.pderichai.text.classification

import breeze.linalg.{DenseVector, Vector}
import breeze.stats._
import ch.ethz.dal.tinyir.io.ReutersRCVStream

import scala.collection.immutable.TreeMap

/**
  * Created by dericp on 11/14/16.
  */
object SVMMain {
  def main(args: Array[String]): Unit = {

    val docs = new ReutersRCVStream("src/main/resources/train").stream
    val tokens = docs.flatMap(_.tokens).toSet
    val allTokensMap = TreeMap(tokens.zipWithIndex.map { case (t, i) => (t, 0) }.toMap.toArray: _*)
    val cat = "USA"
    var step = 1
    var theta = DenseVector.zeros[Double](178069)
    val lambda = 0.001

    for (i <- util.Random.shuffle(0 to docs.size)) {
      val doc = docs(i)
      val docTokens = doc.tokens.groupBy(identity).map { case (term, termList) => (term, termList.size) }

      val tokensMap = allTokensMap.map { case (token, count) => (token, docTokens.getOrElse(token, 0)) }

      val x = Vector(tokensMap.toList.map { case (k, v) => v.toDouble }.toArray: _*)
      val y = if(doc.codes.contains(cat)) 1 else -1
      //println("x: " + x)
      //println("y: " + y)

      theta = SVM.updateStep(theta, new SVM.DataPoint(x, y), lambda, step)
      step = step + 1

      println("step is: " + step)
      //println("theta is: " + theta)
    }

  }

}
