package com.gmail.pderichai.text.classification
import scala.collection.mutable
//import com.gmail.pderichai.text.classification.Document
//import com.gmail.pderichai.text.classification.Category

class NaiveBayes(docCollection: mutable.Set[Document], docCodes: mutable.Map[String, Category]) {
  def probC(category: Category): Double = {
    category.documents.size / docCollection.size.toDouble
  }

  def probWC(word: String, category: Category): Double = {
    val dc = category.documents
    val numerator = dc.map(_.termFreq.getOrElse(word, 0) + 1).sum
    val vocabSize = dc.flatMap(_.termFreq.keys).distinct.size
    val denominator = dc.map(_.length).sum + vocabSize
    numerator / denominator.toDouble
  }

  def logPDC(document: Document, category: Category): Double = {
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
  }
}
