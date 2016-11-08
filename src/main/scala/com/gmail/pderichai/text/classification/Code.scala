package com.gmail.pderichai.text.classification

import scala.collection.mutable.ListBuffer

class Code(val categoryCode: String) {

  var termFreq = scala.collection.mutable.Map.empty[String, Int]
  var documents = new ListBuffer[Document]()

  def addDoc(doc: Document): Unit = {
    documents += doc
    for (term <- doc.termFreq.keys) {
      termFreq(term) = termFreq.getOrElse(term, 0) + doc.termFreq.getOrElse(term, 0)
    }
  }
}