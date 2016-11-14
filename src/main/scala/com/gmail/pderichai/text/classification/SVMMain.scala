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
    /*val normal01 = distributions.Gaussian(0, 1)
    val vec = DenseVector.rand(5, normal01)
    val x = Vector[Double](1, 1, 1, 1, 1)
    val y = 1
    val lambda = 0.5
    val step = 50
    println(vec)
    println(SVM.updateStep(vec, new SVM.DataPoint(x, y), lambda, step))*/
    def docs = new ReutersRCVStream("src/main/resources/train").stream
    val tokens = docs.flatMap(_.tokens).toSet

    val doc = docs.iterator.next()

    var tokensMap = TreeMap(tokens.zipWithIndex.map{case(t, i) => (t, 0)}.toMap.toArray:_*)

    //println(tokensMap.size)

    val doctokens = doc.tokens.groupBy(identity).map{case (term, termList) => (term, termList.size)}

    tokensMap = tokensMap.map{case(token, count) => (token, doctokens.getOrElse(token, 0))}

    /*for (token <- doc.tokens) {
      println("working on token: " + token)
      // tokensMap = tokensMap.map{case(token, c) => (token, c + 1)}

      println("new token value: " + tokensMap(token))
    }*/

    val theta = DenseVector.zeros[Double](178069)
    val x = Vector(tokensMap.toList.map{case(k, v) => v.toDouble}.toArray:_*)
    val y = 1
    val lambda = 0.001
    val step = 50
    println(SVM.updateStep(theta, new SVM.DataPoint(x, y), lambda, step).findAll(_ > 0))

}

}
