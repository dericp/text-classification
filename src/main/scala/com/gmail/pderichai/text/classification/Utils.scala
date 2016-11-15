package com.gmail.pderichai.text.classification

import breeze.linalg.DenseVector
import ch.ethz.dal.tinyir.processing.XMLDocument

import scala.io.Source

/**
  * Created by dericp on 11/14/16.
  */
object Utils {
  val STOP_WORDS = Source.fromFile("src/main/resources/stop-words.txt").getLines.toSet

  def pruneStopWords(termFreq: Map[String, Int]): Map[String, Int] = {
    termFreq.filterKeys(!STOP_WORDS.contains(_))
  }

  def getTermFrequencies(doc: XMLDocument): Map[String, Int] = {
    Utils.pruneStopWords(doc.tokens.groupBy(identity).mapValues(termList => termList.size))
  }

  def getCodes(): Set[String] = {
    val industryCodes = Source.fromFile("src/main/resources/codes/industry_codes.txt").getLines().map(line => line.split("\t")(0)).toSet
    val regionCodes = Source.fromFile("src/main/resources/codes/region_codes.txt").getLines().map(line => line.split("\t")(0)).toSet
    val topicCodes = Source.fromFile("src/main/resources/codes/topic_codes.txt").getLines().map(line => line.split("\t")(0)).toSet
    industryCodes union regionCodes union topicCodes
  }

  // Takes an XMLDocument doc
  // Returns a Document representing doc
  def shortenContent(doc: XMLDocument): Document = {
    val termFreqs = topKTermFreq(doc, 60)
    return new Document(termFreqs, doc.codes, doc.tokens.filter(keepWord(_, termFreqs)), doc.ID)
  }

  // Takes an XMLDocument
  // Returns the term frequency map of the document, deleting stop words and keeping only the top k
  def topKTermFreq(doc: XMLDocument, k: Int): Map[String, Int] = {
    collection.immutable.ListMap(getTermFrequencies(doc).toList.sortBy{-_._2}:_*).take(k)
  }

  // Takes a String s and the term frequencies for a document
  // Returns whether or not the doc should store the word
  def keepWord(s: String, termFreqs: Map[String, Int]): Boolean = {
    val stops = !STOP_WORDS.contains(s)
    val terms = termFreqs.keySet.contains(s)
    return stops && terms
  }

  def getTopTerms(docs: Stream[XMLDocument], numTerms: Int): Set[String] = {
    pruneStopWords(docs.flatMap(_.tokens).groupBy(identity).mapValues(l => l.size)).toSeq.sortBy(-_._2).take(numTerms).map((t) => t._1).toSet
  }

  def getFeatureVector(docTermFreq: Map[String, Int], emptyFeatureVector: DenseVector[Double], termToIndexInFeatureVector: Map[String, Int]): DenseVector[Double] = {
    docTermFreq.foreach { case (term, freq) => emptyFeatureVector(termToIndexInFeatureVector.get(term).get) = freq.toDouble }
    emptyFeatureVector
  }
}
