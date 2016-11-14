package com.gmail.pderichai.text.classification

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
}
