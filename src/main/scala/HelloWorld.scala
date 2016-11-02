import ch.ethz.dal.tinyir.io.ReutersRCVStream
import ch.ethz.dal.tinyir.processing.{ReutersRCVParse, Tokenizer, XMLDocument}
import com.gmail.pderichai.text.classification.Document

import scala.collection.mutable

object HelloWorld {

  def main(args: Array[String]): Unit = {

    val reuters = new ReutersRCVStream("src/main/resources/train")
    val docStream = reuters.stream
    /*val documents =
      for {
        doc <- docStream
      } yield new Document(doc)

    val categories =
      for {
        doc <- documents
        for {

      }*/

    var docs = scala.collection.mutable.Set.empty[Document]
    var categories = scala.collection.mutable.Map.empty

    for (doc <- docStream) {
      docs.new Document(doc)

    }

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