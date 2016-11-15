import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import java.io._
import com.gmail.pderichai.text.classification.{Code, Document, NaiveBayes, Utils}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Main {
  def main(args: Array[String]): Unit = {
    // Map: docID -> document
    def docs = new ReutersRCVStream("src/main/resources/train").stream
//    val iter = docs.iterator
//    val doc1 = iter.next()
//    val doc2 = iter.next()
    val codesToDocIDs = scala.collection.mutable.Map.empty[String, mutable.Seq[Int]].filter(_._2.size > 40)
    val docIDToDoc = scala.collection.mutable.Map.empty[Int, Document]
    val vocabSize = docIDToDoc.values.map(_.tokens).toSet.size

    for (doc <- docs) {
      docIDToDoc += doc.ID -> shortenContent(doc)
      for (code <- doc.codes) {
        codesToDocIDs += code -> codesToDocIDs.getOrElse(code, mutable.Seq.empty[Int]).:+(doc.ID)
      }
    }

    println("finished training")

    def validationDocs = new ReutersRCVStream("src/main/resources/validation").stream
    val f1Vals = ListBuffer.empty[Double]
    val naiveBayes = new NaiveBayes(docIDToDoc, codesToDocIDs, vocabSize, -50) /*
    var i = 0
    var prevLen = 0;

    for (doc <- validationDocs) {
      if (i <= 300) {
        val foundCats = naiveBayes.catsGivenDoc(shortenContent(doc), -600)
        val correctCats = doc.codes
        val score = docF1Score(foundCats, correctCats)
        println("i = " + i + ", score = " + score)
        f1Vals += score
        i += 1
        prevLen = f1Vals.size
        println()
      }
    }
    println()
    println("f1 score: " + algF1Score(f1Vals))
    println("finished f1vals = " + f1Vals)
*/


    def testDocs = new ReutersRCVStream("src/main/resources/test").stream

    /* WRITE TO FILE */
    val file = new File("ir-2016-project-13-nb.txt")
    val bw = new BufferedWriter(new FileWriter(file))

    for (doc <- testDocs) {
      bw.write("" + doc.ID)
      val categories = naiveBayes.catsGivenDoc(shortenContent(doc), -510)
      categories.foreach(c => bw.write(" " + c))
      bw.write("\n")
    }

    // close buffered writer
    bw.close()
  }

  // Takes an XMLDocument doc
  // Returns a Document representing doc
  def shortenContent(doc: XMLDocument): Document = {
    val termFreqs = termFreq(doc)
    return new Document(termFreqs, doc.codes, doc.tokens.filter(keepWord(_, termFreqs)), doc.ID)
  }

  // Takes an XMLDocument
  // Returns the term frequency map of the document, deleting stop words and keeping only the top k
  // TODO: top k?
  def termFreq(doc: XMLDocument): Map[String, Int] = {
    // Utils.getTermFrequencies(doc).toList.sortBy{_._2:_*}.take(45)
    collection.immutable.ListMap(doc.tokens.filter(!Utils.STOP_WORDS.contains(_)).groupBy(identity)
      .mapValues(l => l.length).toList.sortBy{-_._2}:_*).take(60)
  }

  // Takes a String s and the term frequencies for a document
  // Returns whether or not the doc should store the word
  def keepWord(s: String, termFreqs: Map[String, Int]): Boolean = {
    val stops = !Utils.STOP_WORDS.contains(s)
    val terms = termFreqs.keySet.contains(s)
    return stops && terms
  }

  // Takes the Set of categories for this doc found by our algorithm and the
  // Set of categories that are correct for this doc
  // Returns F1 score for doc = 2PR / (P + R)
  def docF1Score(foundCats: scala.collection.Set[String], correctCats: Set[String]): Double = {
    // println("foundCats size: " + foundCats.size)
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
