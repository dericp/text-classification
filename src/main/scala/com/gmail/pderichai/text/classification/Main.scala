import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import com.gmail.pderichai.text.classification.{Code, Document, NaiveBayes}
import scala.collection.mutable

object Main {
  val STOP_WORDS = Set("and", "but", "of", "from", "by", "because", "he", "she", "if", "a", "about")

  def main(args: Array[String]): Unit = {

    // Map: docID -> document
    def docs = new ReutersRCVStream("src/main/resources/train").stream
    // zipWithIndex.map{case(xmlDoc, index) => (xmlDoc.ID, shortenContent(xmlDoc))}
    val doc1 = docs.head
    // val cats = docs.map(_.codes).foldLeft(Set[String]())((b,a) => a ++ b)
    val codesToDocIDs = scala.collection.mutable.Map.empty[String, mutable.Seq[Int]]
    val docIDToDoc = scala.collection.mutable.Map.empty[Int, Document]

    for (doc <- docs) {
      docIDToDoc += doc.ID -> shortenContent(doc)
      for (code <- doc.codes) {
        codesToDocIDs += code -> codesToDocIDs.getOrElse(code, mutable.Seq.empty[Int]).:+(doc.ID)
      }
    }

    println("found codes" + NaiveBayes.catsGivenDoc(docIDToDoc, doc1.ID, 0.05, codesToDocIDs))
    println("correct codes" + doc1.codes)
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