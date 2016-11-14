package com.gmail.pderichai.text.classification
import breeze.linalg.SparseVector
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument

/**
  * Created by dericp on 11/14/16.
  */
object LogisticRegressionMain {
  val STOP_WORDS = Set("and", "but", "of", "from", "by", "because", "he", "she", "if", "a", "about")

  // Iterate through randomly?
  // var theta = breeze.linalg.SparseVector.zeros[Double](thetaLen)

  // Need to define a theta for each category
  // Start with theta all zeros (0 for each term freq)
  // Go through every document

  // var theta = SMap(Map.empty[String, Double])
  def main(args: Array[String]): Unit = {
    def docs = new ReutersRCVStream("src/main/resources/train").stream
    // Don't pull just top 40
    val iter = docs.iterator
    val doc = shortenContent(iter.next)
    val doc2 = shortenContent(iter.next())


    // Map from term -> vector index
    // FILTER?? filter(!STOP_WORDS.contains(_))
    val termToIndex = docs.flatMap(_.tokens).distinct.zipWithIndex.toMap
    val numTerms = termToIndex.size
    val docVec1 = SparseVector.zeros[Double](numTerms)
    // CHANGE TO USE .MAP?
    doc.termFreq.foreach{case(term, freq) => docVec1(termToIndex.get(term).get) = freq.toDouble}
    val docVec2 = SparseVector.zeros[Double](numTerms)
    doc2.termFreq.foreach{case(term, freq) => docVec2(termToIndex.get(term).get) = freq.toDouble}
    var thetaVec = SparseVector.zeros[Double](numTerms)
    val cat = "USA"

    println("docvec1 " + docVec1)
    println("docvec2 " + docVec2)
    println("diff " + docVec1.-(docVec2))

    // Set (category --> theta of that category)
    // val catToTheta = Map.empty[String, SparseVector[Double]]

    var step = 1;

    for (xmlDoc <- docs) {
      val doc = shortenContent(xmlDoc)
      val docVec = SparseVector.zeros[Double](termToIndex.size)
      doc.termFreq.foreach{case(term, freq) => docVec(termToIndex.get(term).get) = freq.toDouble}
      thetaVec = LogisticRegression.updateTheta(thetaVec, docVec, doc.codes.contains(cat), step)
      step += 1
      // println("step: " + step)
    }

    println("prob doc1: " + LogisticRegression.logistic(docVec1, thetaVec))
    var str = ""
    if (doc2.codes.contains(cat)) str = "not "
    println("doc2 does " + str + "contain " + cat)
    println("prob doc2: " + LogisticRegression.logistic(docVec2, thetaVec))

    // println("final thetaVec: " + thetaVec)

    // For each document
    // Get term freq of that document
    // Determine if code is in document
    // Pass in values to update theta


  }


  // REMOVE THIS IT'S IN MAIN
  def shortenContent(doc: XMLDocument): Document = {
    val termFreqs = termFreq(doc)
    return new Document(termFreqs, doc.codes, doc.tokens.filter(keepWord(_, termFreqs)), doc.ID)
  }

  def termFreq(doc: XMLDocument): Map[String, Int] = {
    collection.immutable.ListMap(doc.tokens.filter(!STOP_WORDS.contains(_)).groupBy(identity)
      .mapValues(l => l.length).toList.sortBy{_._2}:_*)
  }

  def keepWord(s: String, termFreqs: Map[String, Int]): Boolean = {
    val stops = !STOP_WORDS.contains(s)
    val termsss = termFreqs.keySet.contains(s)
    return stops && termsss
  }
}
