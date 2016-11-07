import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import com.gmail.pderichai.text.classification.{Code, Document, NaiveBayes}
import scala.collection.mutable

object Main {

  def main(args: Array[String]): Unit = {
    // TODO: change bayesmain to return a list of top k codes
    // Parse Codes?? Do we want to be able to tell what type each code is?
    // val topics = processCodes(scala.io.Source.fromFile("src/main/resources/codes/topic_codes"))

    // Train documents for Naive Bayes
    // val reuters = new ReutersRCVStream("src/main/resources/train")
    val trainingDocStream1 = new ReutersRCVStream("src/main/resources/train").stream
    val trainingDocStream2 = new ReutersRCVStream("src/main/resources/train").stream
    val trainingDocStream3 = new ReutersRCVStream("src/main/resources/train").stream
    val doc = trainingDocStream3.iterator.next()


    println(NaiveBayes.probOfDocGivenCat(trainingDocStream1, trainingDocStream2, doc, doc.codes.iterator.next()))


    /*val docs = mutable.Set.empty[Document]
    val codes = mutable.Map.empty[String, Code]

    for (doc <- trainingDocStream) {
      val curDoc = new Document(doc)
      val curCodes = curDoc.codes
      for (code <- curCodes) {
        codes.getOrElse(code, new Code(code)).addDoc(curDoc)
      }
      docs add curDoc
    }*/

    // What do we want to average for testing?  Some threshold for % likelihood?
    // TODO: n-fold validation changing some parameter
    /*val naiveBayes = new NaiveBayes(docs, codes)
    val validationDocStream = new ReutersRCVStream("src/main/resources/validation").stream
    //val pw = new PrintWriter(new File("src/main/resources/validation_codes"))
    var percNum = 0
    var percDen = 0

    // For now just printing
    for (doc <- trainingDocStream) {
      val curDoc = new Document(doc)
      val curCodes = curDoc.codes
      val topCode = naiveBayes.bayesMain(curDoc)
      if (!curCodes.contains(topCode)) {
        percNum += 1
      }
      percDen += 1
    }

    println(100.0 * percNum / percDen)*/

    // pw.close


    /*val doc = docIter.next()
    println(doc.codes.toString())
    println(doc.name)
    println(doc.ID)
    println(doc.title)*/

    //val tokens = Tokenizer.tokenize(testdoc.content)
    //println(tokens.mkString(","))
    //println(testdoc.codes.size)
    //val copyright = testdoc.doc.getDocumentElement.getElementsByTagName("copyright")
    //println(testdoc.doc.getDocumentElement.getElementsByTagName("copyright").toString())
    //println(doc.codes.size)
  }
}