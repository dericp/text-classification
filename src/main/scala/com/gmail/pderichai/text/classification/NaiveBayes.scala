package com.gmail.pderichai.text.classification
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable

/*class NaiveBayes(docCollection: mutable.Set[Document], docCodes: mutable.Map[String, Code]) {
  (def probC(category: Code): Double = {
    category.documents.size / docCollection.size.toDouble
  }

  def probWC(word: String, category: Code): Double = {
    val dc = category.documents
    val numerator = dc.map(_.termFreq.getOrElse(word, 0) + 1).sum
    val vocabSize = dc.flatMap(_.termFreq.keys).distinct.size
    val denominator = dc.map(_.length).sum + vocabSize
    numerator / denominator.toDouble
  }

  def logPDC(document: Document, category: Code): Double = {
    val tf = document.termFreq
    val pwc = tf.keys.map(w => (w, probWC(w, category))).toMap
    pwc.map{ case(k, v) => tf.getOrElse(k, 0) * Math.log(v) }.sum
  }

  // TODO: change this method to return a list of top k codes
  // Returns the Category with the highest probability
  def bayesMain(document: Document): String = {
    // need the collection of docs, this is mock
    val categoryProbs = docCodes.map{case(name, code) => (name, Math.log(probC(code)) + logPDC(document, code))}
    categoryProbs.maxBy(_._2)._1
  }*/

object NaiveBayes {
  //val STOP_WORDS = Set("and", "but", "of", "from", "by", "because", "he", "she", "if", "a", "about")

  // Returns the probability of a given category
  def probC(docs: Seq[Document], cat: String): Double = {
    docs.filter(_.codes(cat)).length / docs.length.toDouble
  }

  // Returns a map from word w --> probability of w in the given category
  def probOfWGivenCMany(docs: Seq[Document], cat: String): Map[String, Double] = {
    val vocabSize = docs.flatMap(_.tokens).distinct.length
    // val vocabSize = docs.map(_.termFreq.values).groupBy(identity)
    val tks = docs.filter(_.codes(cat)).flatMap(_.tokens)
    // val tks = docs.filter(_.codes(cat)).flatMap(_.tokens)
    val denominator = tks.length.toDouble + vocabSize
    // keep sparseness, no normalization here
    tks.groupBy(identity).mapValues(l=>(l.length + 1) / denominator)
  }

  def probOfDocGivenCat(docs: Seq[Document], probOfWGivenCMany: Map[String, Double], doc: Document, cat: String): Double = {
    val probOfC = probC(docs, cat)
    val termFreq = doc.tokens.groupBy(identity).mapValues(l => l.length)
    println("prob of C " + probOfC)
    Math.log(probOfC) + termFreq.map{case(term, i)=>(term, termFreq.getOrElse(term, 0) * Math.log(probOfWGivenCMany.getOrElse(term, 0)))}.values.sum
  }

  def catsGivenDoc(docs: Seq[Document], doc: Document, threshold: Double, cats: Set[String]): Set[String] = {
    val catProbs = cats.zipWithIndex.map{case (cat, index) => (cat, probOfDocGivenCat(docs, probOfWGivenCMany(docs, cat), doc, cat))}
    val thresholdProbs = catProbs.filter{case (cat, prob) => prob >= threshold}.map(_._1)
    if (thresholdProbs.size > 0) thresholdProbs else Set(catProbs.maxBy(_._2)._1)
  }
}
