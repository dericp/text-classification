package com.gmail.pderichai.text.classification
import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

import scala.collection.mutable
// Currently: f1 = 0.41434146680965195
// TODO: stop storing tokens in Document
// What do we do with the validation set?  What do we do with words that are in the validation set but not the training set?
class NaiveBayes(docs: mutable.Map[Int, Document], cats: mutable.Map[String, mutable.Seq[Int]], vocabSize: Int, missingTermWeight: Double) {
  val numTrainingDocs = docs.keySet.size.toDouble
  val catToWordProbs = mutable.Map.empty[String, Map[String, Double]]
  // val totalNumTokens = docs.map(_._2.tokens).sum
  // val allTermFreq = docs.map(_._2.tokens).flatten.toSeq.groupBy(identity).mapValues(l => l.size + 1)

  // Returns the probability of a given category
  def probC(cat: String): Double = cats.getOrElse(cat, Seq.empty).size / numTrainingDocs

  def probNotC(cat: String): Double = 1 - probC(cat)

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

  // TODO: Make sure - doesn't change cats
  // def probOfWGivenNotCMany(cat: String): Map[String, Double] = {
//    // all tokens in docs - tokens from docs that are in category
//    // val tks = allTokens.diff(cats(cat).map(docs(_).tokens).flatten)
//    val catTF = cats(cat).flatMap(docs(_).tokens).map(l => l.length + 1)
//    allTermFreq.map{case(term, freq) => (term, (freq - catTF(term)))}
//    // tks = all
//    val denominator = tks.size.toDouble + vocabSize
//    val result = tks.groupBy(identity).mapValues(l => Math.log((l.size + 1) / denominator))
//    // catToWordProbs(cat) = result
//    result
    // remove cat
    // cats.keySet.map(probOfWGivenCMany())
  // }

  def probOfDocGivenCat(probOfWGivenCMany: Map[String, Double], doc: Document, cat: String): Double = {
    val probOfC = probC(cat)
    val termFreq = doc.termFreq
    // termFreq.getOrElse(term, 0) --> _.get
    Math.log(probOfC) + termFreq.map{case(term, i)=>termFreq.getOrElse(term, 0) * probOfWGivenCMany.getOrElse(term, missingTermWeight)}.sum
  }

//  def probOfDocGivenNotCat(probOfWgivenNotCMany: Map[String, Double], doc: Document, cat: String): Double = {
//    println("hey")
//    val probOfNotC = probNotC(cat)
//    val termFreq = doc.termFreq
//    Math.log(probOfNotC) + termFreq.map{case(term, i)=>(termFreq.getOrElse(term, 0) * probOfWgivenNotCMany.getOrElse(term, missingTermWeight))}.sum
//  }

  // def test(abc: Set[String]): Unit = {}

  def catsGivenDoc(doc: Document, threshold: Double): collection.Set[String] = {
    // cats.keySet.filter{case(cat) => probOfDocGivenCat(probOfWGivenCMany(cat), doc, cat) > probOfDocGivenNotCat(probOfWGivenNotCMany(cat), doc, cat) }


    val catProbs = cats.keySet.zipWithIndex.map{case (cat, i) => (cat, probOfDocGivenCat(probOfWGivenCMany(cat), doc, cat))}.toMap
    // println("catProbs" + catProbs.values)
    val thresholdProbs = catProbs.filter{case (cat, prob) => prob >= threshold}
    //println("thresholdProbs")
    // Currently only returns the top answer because thresholdProbs is empty
//    if (thresholdProbs.size > 5) {
//      val toReturn = thresholdProbs.toSeq.sortBy(-_._2).take(4).map{case(k, v) => k}.toSet
//      println("shortened thresholdProbs: " + toReturn)
//      toReturn
//    } else if (thresholdProbs.size > 0) {
//      val toReturn = thresholdProbs.map(_._1).toSet
//      println("using thresholdProbs: " + toReturn)
//      toReturn
//    } else {
//      val toReturn = catProbs.toSeq.sortBy(-_._2).take(4).map{case(k, v) => k}.toSet
//      println("nothing > threshold: " + toReturn)
//      toReturn
//    }
    // f1 score: 0.5993730359276583
    catProbs.toSeq.sortBy(-_._2).take(4).map{case(k, v) => k}.toSet
  }

  //  def topCodeForDoc(cats: mutable.Map[String, mutable.Seq[Int]], docs: mutable.Map[Int, Document], docID: Int): String = {
  //    cats.map{case(cat, catDocs) => (cat, probOfDocGivenCat(docs, probOfWGivenCMany(docs, cats, cat), docID, cat, cats))}.maxBy(_._2)._1
  //  }
}