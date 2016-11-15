package com.gmail.pderichai.text.classification
import java.io.{BufferedWriter, File, FileWriter}
import ch.ethz.dal.tinyir.io.ReutersRCVStream
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Main class for validating or creating test output document using NaiveBayes.
  *
  */
object NaiveBayesMain {
  // Weight assigned to terms that are in validation or test but not in training set
  val MISSING_TERM_RATE = -20
  val CODE_THRESHOLD = 40
  val naiveBayesClassifier = train()

  // Returns a NaiveBayes object that is trained with the documents in src/main/resources/train
  def train() : NaiveBayes = {
    def docs = new ReutersRCVStream("src/main/resources/train").stream
    // Map: code -> docsIDs in code
    val codesToDocIDs = scala.collection.mutable.Map.empty[String, mutable.Seq[Int]].filter(_._2.size > CODE_THRESHOLD)
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

    new NaiveBayes(docIDToDoc, codesToDocIDs, vocabSize, MISSING_TERM_RATE)
  }

  // Validates a NaiveBayes instance using the documents in src/main/resources/validation
  // Prints the average f1 score to output
  def runOnValidationSet(): Unit = {
    def validationDocs = new ReutersRCVStream("src/main/resources/validation").stream
    val f1Vals = ListBuffer.empty[Double]

    for (doc <- validationDocs) {
      val foundCats = naiveBayesClassifier.catsGivenDoc(Utils.shortenContent(doc))
      val correctCats = doc.codes
      val score = docF1Score(foundCats, correctCats)
      f1Vals += score
    }
    println("f1 score: " + algF1Score(f1Vals))
  }

  // Tests a NaiveBayes instance on the documents in src/main/resources/test
  // Creates a document in the following format:
  // [document ID] [category 1] [category 2] [category 3] [category 4]
  def runOnTestSet(): Unit = {
    def testDocs = new ReutersRCVStream("src/main/resources/test").stream
    val file = new File("ir-2016-project-13-nb.txt")
    val bw = new BufferedWriter(new FileWriter(file))

    for (doc <- testDocs) {
      bw.write("" + doc.ID)
      val categories = naiveBayesClassifier.catsGivenDoc(Utils.shortenContent(doc))
      categories.foreach(c => bw.write(" " + c))
      bw.write("\n")
    }
    bw.close()
  }

  // Takes the Set of categories for this doc found by our algorithm and the
  // Set of categories that are correct for this doc
  // Returns F1 score for doc = 2PR / (P + R)
  def docF1Score(foundCats: scala.collection.Set[String], correctCats: Set[String]): Double = {
    val relevantRetrieved = foundCats.count(correctCats(_)).toDouble
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
