import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import com.gmail.pderichai.text.classification.{Code, Document, NaiveBayes}
import scala.collection.mutable

object Main {
  val STOP_WORDS = Set("and", "but", "of", "from", "by", "because", "he", "she", "if", "a", "about")

  def main(args: Array[String]): Unit = {

    // Train documents for Naive Bayes
    // ALL THE SAME:
//    println(doc.name)
//    println(trainingDocStream1.iterator.next().name)
//    println(trainingDocStream1.apply(0).name)

    //    for (doc <- docs) {
    //      for (code <- doc.codes) {
    //        val probOfWGivenCMany = NaiveBayes.probOfWGivenCMany(docs, code)
    //        println("finished computing probOfWGivenCMany")
    //        val probOfDocGivenCat = NaiveBayes.probOfDocGivenCat(docs, probOfWGivenCMany, doc, code)
    //        println(probOfDocGivenCat)
    //      }
    //    }
    //  }

    val docs = new ReutersRCVStream("src/main/resources/train").stream.map(shortenContent)
    val doc1 = docs.head
    val cats = docs.map(_.codes).foldLeft(Set[String]())((b,a) => a ++ b)

    println(NaiveBayes.catsGivenDoc(docs, doc1, 0.3, cats))
  }

  def shortenContent(doc: XMLDocument): Document = {
    val termFreqs = termFreq(doc)
    return new Document(termFreqs, doc.codes, doc.tokens.filter(keepWord(_, termFreqs)))
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

  def docF1Score(foundCats: Set[String], correctCats: Set[String]): Double = {
    // Precision = # relevant items retrieved / # items retrieved
    // Recall = # relevant items retrieved / # relevant items in collection
    // F1 = 2PR / (P + R)
    val relevantRetrieved = foundCats.filter(correctCats(_)).size.toDouble
    val p = relevantRetrieved / foundCats.size
    val r = relevantRetrieved / correctCats.size
    2 * p * r / (p + r)
  }

  def algF1Score(docScores: Set[Double]): Double = {
    docScores.sum / docScores.size
  }
}