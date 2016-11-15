package com.gmail.pderichai.text.classification
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable
class NaiveBayes(docs: mutable.Map[Int, Document], cats: mutable.Map[String, mutable.Seq[Int]], vocabSize: Int, missingTermWeight: Double) {
  val numTrainingDocs = docs.keySet.size.toDouble
  val catToWordProbs = mutable.Map.empty[String, Map[String, Double]]

  // Returns the probability of a given category
  def probC(cat: String): Double = cats.getOrElse(cat, Seq.empty).size / numTrainingDocs

  // Returns a map from word w --> probability of w in the given category
  def probOfWGivenCMany(cat: String): Map[String, Double] = {
    if (catToWordProbs.contains(cat)) {
      catToWordProbs(cat)
    } else {
      val tks = cats.getOrElse(cat, Seq.empty).flatMap(docs(_).tokens)
      val denominator = tks.length.toDouble + vocabSize
      val result = tks.groupBy(identity).mapValues(l => Math.log((l.length + 1) / denominator))
      catToWordProbs(cat) = result
      result
    }
  }

  // Returns the P(document | category)
  def probOfDocGivenCat(probOfWGivenCMany: Map[String, Double], doc: Document, cat: String): Double = {
    val probOfC = probC(cat)
    val termFreq = doc.termFreq
    // termFreq.getOrElse(term, 0) --> _.get
    Math.log(probOfC) + termFreq.map{case(term, i)=>termFreq.getOrElse(term, 0) * probOfWGivenCMany.getOrElse(term, missingTermWeight)}.sum
  }

  // Returns the assigned categories given a document
  def catsGivenDoc(doc: Document): collection.Set[String] = {
    val catProbs = cats.keySet.zipWithIndex.map{case (cat, i) => (cat, probOfDocGivenCat(probOfWGivenCMany(cat), doc, cat))}.toMap
    val thresholdProbs = catProbs.filter{case (cat, prob) => prob >= threshold}
    catProbs.toSeq.sortBy(-_._2).take(4).map{case(k, v) => k}.toSet
  }
}