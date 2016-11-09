import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import com.gmail.pderichai.text.classification.{Code, Document, NaiveBayes}
import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    val docs = new ReutersRCVStream("src/main/resources/train").stream

    for (doc <- docs) {
      for (code <- doc.codes) {
        val probOfWGivenCMany = NaiveBayes.probOfWGivenCMany(docs, code)
        println("finished computing probOfWGivenCMany")
        val probOfDocGivenCat = NaiveBayes.probOfDocGivenCat(docs, probOfWGivenCMany, doc, code)
        println(probOfDocGivenCat)
      }
    }
  }
}