package com.gmail.pderichai.text.classification
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable

object NaiveBayes {

  // Returns the probability of a given category
  def probC(docs: mutable.Map[Int, Document], cats: mutable.Map[String, mutable.Seq[Int]], cat: String): Double = {
    val res = cats.getOrElse(cat, Seq.empty).size / docs.keySet.size.toDouble
    res
  }

  // Returns a map from word w --> probability of w in the given category
  def probOfWGivenCMany(docs: mutable.Map[Int, Document], cats: mutable.Map[String, mutable.Seq[Int]], cat: String): Map[String, Double] = {
    val vocabSize = docs.values.map(_.tokens).toSet.size
    val tks = cats.getOrElse(cat, Seq.empty).map(docs.get(_).get.tokens).flatten
    val denominator = tks.length.toDouble + vocabSize
    val res = tks.groupBy(identity).mapValues(l => Math.log((l.length + 1) / denominator))
    res
  }

  def probOfDocGivenCat(docs: mutable.Map[Int, Document], probOfWGivenCMany: Map[String, Double], docID: Int, cat: String, cats: mutable.Map[String, mutable.Seq[Int]]): Double = {
    val probOfC = probC(docs, cats, cat)
    val termFreq = docs.get(docID).get.termFreq
    val res1 =  termFreq.map{case(term, i)=>(term, termFreq.getOrElse(term, 0) * probOfWGivenCMany.getOrElse(term, -100.0))}
    // println("probOfDocGivenCat partial result: " + res1)
    val res = Math.log(probOfC) + res1.values.sum
    // println("probOfDocGivenCat: " + res)
    res
  }

  def test(abc: Set[String]): Unit = {}

  def catsGivenDoc(docs: mutable.Map[Int, Document], docID: Int, threshold: Double, cats: mutable.Map[String, mutable.Seq[Int]]): collection.Set[String] = {
    val catProbs = cats.keySet.zipWithIndex.map{case (cat, i) => (cat, probOfDocGivenCat(docs, probOfWGivenCMany(docs, cats, cat), docID, cat, cats))}.toMap
    // println("catProbs" + catProbs.values)
    val thresholdProbs = catProbs.filter{case (cat, prob) => prob >= threshold}.map(_._1).toSet
    // Currently only returns the top answer because thresholdProbs is empty
    if (thresholdProbs.size > 0) {
      println("using thresholdProbs")
      thresholdProbs
    } else  {
      println("nothing > threshold")
      Set(catProbs.maxBy(_._2)._1)
    }
  }

  //  def topCodeForDoc(cats: mutable.Map[String, mutable.Seq[Int]], docs: mutable.Map[Int, Document], docID: Int): String = {
  //    cats.map{case(cat, catDocs) => (cat, probOfDocGivenCat(docs, probOfWGivenCMany(docs, cats, cat), docID, cat, cats))}.maxBy(_._2)._1
  //  }
}