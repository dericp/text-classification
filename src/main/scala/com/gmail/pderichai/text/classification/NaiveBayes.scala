package com.gmail.pderichai.text.classification
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable
// Currently: f1 = 0.41434146680965195
// TODO: Cache probabilities that are computed multiple times, stop storing tokens in Document, always return < 5 categories
// TODO: Use n-fold validation (maybe not)
// What do we do with the validation set?  What do we do with words that are in the validation set but not the training set?
class NaiveBayes(docs: mutable.Map[Int, Document], cats: mutable.Map[String, mutable.Seq[Int]], vocabSize: Int, missingTermWeight: Double) {
  val numTrainingDocs = docs.keySet.size.toDouble
  val catToWordProbs = mutable.Map.empty[String, Map[String, Double]]

  // Returns the probability of a given category
  def probC(cat: String): Double = {
    cats.getOrElse(cat, Seq.empty).size / numTrainingDocs
  }

  // Returns a map from word w --> probability of w in the given category
  def probOfWGivenCMany(cat: String): Map[String, Double] = {
    if (catToWordProbs.contains(cat)) {
      catToWordProbs(cat)
    } else {
      val tks = cats.getOrElse(cat, Seq.empty).map(docs.get(_).get.tokens).flatten
      val denominator = tks.length.toDouble + vocabSize
      val result = tks.groupBy(identity).mapValues(l => Math.log((l.length + 1) / denominator))
      catToWordProbs(cat) = result
      result
    }
  }

  def probOfDocGivenCat(probOfWGivenCMany: Map[String, Double], doc: Document, cat: String): Double = {
    val probOfC = probC(cat)
    val termFreq = doc.termFreq
    // termFreq.getOrElse(term, 0) --> _.get
    Math.log(probOfC) + termFreq.map{case(term, i)=>(termFreq.getOrElse(term, 0) * probOfWGivenCMany.getOrElse(term, missingTermWeight))}.sum
  }

  def test(abc: Set[String]): Unit = {}

  def catsGivenDoc(doc: Document, threshold: Double): collection.Set[String] = {
    val catProbs = cats.keySet.zipWithIndex.map{case (cat, i) => (cat, probOfDocGivenCat(probOfWGivenCMany(cat), doc, cat))}.toMap
    // println("catProbs" + catProbs.values)
    val thresholdProbs = catProbs.filter{case (cat, prob) => prob >= threshold}
    //println("thresholdProbs")
    // Currently only returns the top answer because thresholdProbs is empty
    if (thresholdProbs.size > 50) {
      // modifying > 40, taking top 40 docs f1 score: 0.3836954505053356
      // modifying > 50, taking top 50 docs f1 score: 0.38489195313432195
      // modifying > 50, taking top 20 docs f1 score:
      thresholdProbs.toSeq.sortBy(_._2).take(20).map{case(k, v) => k}.toSet
    } else
    if (thresholdProbs.size > 0) {
      val toReturn = thresholdProbs.map(_._1).toSet
      // println("using thresholdProbs: " + toReturn)
      toReturn
    } else  {
      val toReturn = Set(catProbs.maxBy(_._2)._1)
      // println("nothing > threshold" + toReturn)
      toReturn
    }
  }

  //  def topCodeForDoc(cats: mutable.Map[String, mutable.Seq[Int]], docs: mutable.Map[Int, Document], docID: Int): String = {
  //    cats.map{case(cat, catDocs) => (cat, probOfDocGivenCat(docs, probOfWGivenCMany(docs, cats, cat), docID, cat, cats))}.maxBy(_._2)._1
  //  }
}