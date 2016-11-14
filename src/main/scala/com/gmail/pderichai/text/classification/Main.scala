import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import com.gmail.pderichai.text.classification.{Code, Document, NaiveBayes}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Main {
  val STOP_WORDS = Set("and", "but", "of", "from", "by", "because", "he", "she", "if", "a", "about")

  def main(args: Array[String]): Unit = {
    // Map: docID -> document
    def docs = new ReutersRCVStream("src/main/resources/train").stream
//    val iter = docs.iterator
//    val doc1 = iter.next()
//    val doc2 = iter.next()
    val codesToDocIDs = scala.collection.mutable.Map.empty[String, mutable.Seq[Int]]
    val docIDToDoc = scala.collection.mutable.Map.empty[Int, Document]
    val vocabSize = docIDToDoc.values.map(_.tokens).toSet.size

    for (doc <- docs) {
      docIDToDoc += doc.ID -> shortenContent(doc)
      for (code <- doc.codes) {
        codesToDocIDs += code -> codesToDocIDs.getOrElse(code, mutable.Seq.empty[Int]).:+(doc.ID)
      }
    }

//    println("found codes" + NaiveBayes.catsGivenDoc(docIDToDoc, doc1.ID, -400, codesToDocIDs))
//    println("correct codes" + doc1.codes)
//    println()
//    println("found codes" + NaiveBayes.catsGivenDoc(docIDToDoc, doc2.ID, -400, codesToDocIDs))
//    println("correct codes" + doc2.codes)

    println("finished training")

    def validationDocs = new ReutersRCVStream("src/main/resources/validation").stream



    // val missingTermRates = Set(-40.0, -50.0, -60.0, -30.0, -80.0, -100.0, -20.0)  WINNER = -50
    // val thresholds = Set(-495, -500, -505, -510, -490)  WINNER = -510
    // 0.4147984633159311
    val f1Vals = ListBuffer.empty[Double]
    val naiveBayes = new NaiveBayes(docIDToDoc, codesToDocIDs, vocabSize, -50)
    var i = 0
    var prevLen = 0;
    for (doc <- validationDocs) {
      if (i <= 20) {
        val foundCats = naiveBayes.catsGivenDoc(shortenContent(doc), -510)
        val correctCats = doc.codes
        val score = docF1Score(foundCats, correctCats)
        println("i = " + i + ", score = " + score)
        f1Vals += score
        if (f1Vals.size == prevLen) {
          println("!!! Size did not change!!! f1Vals = " + f1Vals +  ", correct cats = " + correctCats + ", found cats = " + foundCats)
        }
        if (score.isNaN) {
          println("NaN!  foundCats = " + foundCats.size + ", correctCats = " + correctCats)
        }
        // println("Added docF1Score to f1Vals. i = " + i + ", docF1Score =  " + score + ", " + "f1Vals size = " + f1Vals.size)
        // println()
        i += 1
        prevLen = f1Vals.size
        // if (i % 100 == 0) println("i = " + i)
        println()
      }

    }
    // Why is sum of algF1Score (should be # entries) sometimes less than # docs looked at??
    println()
    println("f1 score: " + algF1Score(f1Vals))
    println("finished f1vals = " + f1Vals)
  }

  def shortenContent(doc: XMLDocument): Document = {
    val termFreqs = termFreq(doc)
    return new Document(termFreqs, doc.codes, doc.tokens.filter(keepWord(_, termFreqs)), doc.ID)
  }

  def termFreq(doc: XMLDocument): Map[String, Int] = {
    // val tokens = Tokenizer.tokenize(doc.content)
    collection.immutable.ListMap(doc.tokens.filter(!STOP_WORDS.contains(_)).groupBy(identity)
      .mapValues(l => l.length).toList.sortBy{_._2}:_*).take(40)
  }

  def keepWord(s: String, termFreqs: Map[String, Int]): Boolean = {
    val stops = !STOP_WORDS.contains(s)
    val termsss = termFreqs.keySet.contains(s)
    return stops && termsss
  }

  def docF1Score(foundCats: scala.collection.Set[String], correctCats: Set[String]): Double = {
    // Precision = # relevant items retrieved / # items retrieved
    // Recall = # relevant items retrieved / # relevant items in collection
    // F1 = 2PR / (P + R)
    val relevantRetrieved = foundCats.filter(correctCats(_)).size.toDouble
    val p = relevantRetrieved / foundCats.size
    val r = relevantRetrieved / correctCats.size
    2 * p * r / (p + r)
  }

  def algF1Score(docScores: ListBuffer[Double]): Double = {
    val sum = docScores.sum
    println("sum: " + sum)
    val size = docScores.size
    println("size: " + size)
    sum / size
  }
}