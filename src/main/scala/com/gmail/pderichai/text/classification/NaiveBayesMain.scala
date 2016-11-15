package com.gmail.pderichai.text.classification

import java.io.{BufferedWriter, File, FileWriter}

import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.XMLDocument

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Isa on 11/15/2016.
  */
object NaiveBayesMain {
  val naiveBayesClassifier = train()

  def train() : NaiveBayes = {
    def docs = new ReutersRCVStream("src/main/resources/train").stream
    // Map: code -> docsIDs in code
    val codesToDocIDs = scala.collection.mutable.Map.empty[String, mutable.Seq[Int]].filter(_._2.size > 40)
    // Map: docID -> document
    val docIDToDoc = scala.collection.mutable.Map.empty[Int, Document]
    val vocabSize = docIDToDoc.values.map(_.tokens).toSet.size

    // Training
    for (doc <- docs) {
      docIDToDoc += doc.ID -> Utils.shortenContent(doc)
      for (code <- doc.codes) {
        codesToDocIDs += code -> codesToDocIDs.getOrElse(code, mutable.Seq.empty[Int]).:+(doc.ID)
      }
    }

    new NaiveBayes(docIDToDoc, codesToDocIDs, vocabSize, -10)
  }

  def runOnValidationSet(): Unit = {
    // Validation
    def validationDocs = new ReutersRCVStream("src/main/resources/validation").stream

    val f1Vals = ListBuffer.empty[Double]
    //var i = 0

    for (doc <- validationDocs) {
      //if (i <= 300) {
      val foundCats = naiveBayesClassifier.catsGivenDoc(Utils.shortenContent(doc), -1000)
      val correctCats = doc.codes
      val score = docF1Score(foundCats, correctCats)
      //println("i = " + i + ", score = " + score)
      f1Vals += score
      //i += 1
      //println()
      //}
    }
    println()
    println("f1 score: " + algF1Score(f1Vals))
    //println("missingTermWeight: -100")
    println("finished f1vals = " + f1Vals)
  }


  def runOnTestSet(): Unit = {
    // Test output
    def testDocs = new ReutersRCVStream("src/main/resources/test").stream

    /* WRITE TO FILE */
    val file = new File("ir-2016-project-13-nb.txt")
    val bw = new BufferedWriter(new FileWriter(file))

    for (doc <- testDocs) {
      bw.write("" + doc.ID)
      val categories = naiveBayesClassifier.catsGivenDoc(Utils.shortenContent(doc), -510)
      categories.foreach(c => bw.write(" " + c))
      bw.write("\n")
    }

    // close buffered writer
    bw.close()
  }


  // Takes the Set of categories for this doc found by our algorithm and the
  // Set of categories that are correct for this doc
  // Returns F1 score for doc = 2PR / (P + R)
  def docF1Score(foundCats: scala.collection.Set[String], correctCats: Set[String]): Double = {
    val relevantRetrieved = foundCats.filter(correctCats(_)).size.toDouble
    if (relevantRetrieved == 0) return 0
    val p = relevantRetrieved / foundCats.size
    val r = relevantRetrieved / correctCats.size
    2 * p * r / (p + r)
  }

  // Takes the Set of f1 scores for a group of documents
  // Returns average f1 score
  def algF1Score(docScores: ListBuffer[Double]): Double = {
    docScores.sum / docScores.size
  }

}
