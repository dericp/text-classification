package com.gmail.pderichai.text.classification

import ch.ethz.dal.tinyir.processing.{Tokenizer, XMLDocument}

/**
  * Wrapper class for XMLDocument Stream generated by ReutersRCVStream
  */
class Document(val XMLDoc: XMLDocument) {

  val tokens = Tokenizer.tokenize(XMLDoc.content)
  val termFreq = tokens.groupBy(identity).mapValues(l => l.length)
  val length = termFreq.map(_._2).sum
  val codes = XMLDoc.codes



  //var categories = document.doc.

  // , var prevCategories: scala.collection.mutable.Map[String, Category]
  /*var categories = scala.collection.mutable.LinkedList.empty[String];

  private val document = Source.fromFile(filePath).mkString.split("[ .,;:?!\t\n\r\f]+");
  var termFrequencies = document.groupBy(identity).mapValues(l => l.length);
  var length = termFrequencies.map(_._2).sum;

  println("length " + length)
  println("termFrequencies " + termFrequencies)

  ////// SKIPPED CATEGORY ASSIGNMENTS - see library
  // Constructor

  private def parseDoc() {
    // add term frequencies in categories
    // for each category
    val c = new Category();
    termFrequencies.foreach{ case (term, count) => c.incrementTF(term, count) }
    // c.documents.append(self);  WHAT IS 'THIS'
  } */
}