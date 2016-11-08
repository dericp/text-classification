import scala.io.Source
import scala.math.{log}
import scala.collection._

object HelloWorld {


  def parseDoc(filePath: String): Unit = {
    val document = Source.fromFile(filePath).mkString.split("[ .,;:?!\t\n\r\f]+")
    val termFrequencies = document.groupBy(identity).mapValues(_.length)
    val docLength = termFrequencies.map(_._2).sum
    //val categories = get categories
    val vocabSize = termFrequencies.size

  }

  def parseTopicCodes(): Unit = {
    val filePath = "src/main/resources/codes/topic_codes.txt"
    val document = Source.fromFile(filePath).getLines().filter(!_.startsWith(";"))
    for (line <- document) {
      val code = line.split('\t')(0)
    }
  }


  // naive bayes

  def probC(category: Category): Double = {
    // need a collection of documents, this is mock
    val documents = immutable.Seq.empty
    category.documents.size / documents.size.toDouble
  }

  def probWC(word: String, category: Category): Double = {
    val dc = category.documents
    val numerator = dc.map(_.termFrequencies.getOrElse(word, 0) + 1).sum
    val vocabSize = dc.flatMap(_.termFrequencies.keys).distinct.size
    val denominator = dc.map(_.length).sum + vocabSize
    numerator / denominator.toDouble
  }


  def logPDC(document: Document, category: Category): Double = {
    val tf = document.termFrequencies
    val pwc = tf.keys.map(w => (w, probWC(w, category))).toMap
    pwc.map{ case(k, v) => tf.getOrElse(k, 0) * log(v) }.sum
  }

  def bayesMain(document: Document): Category = {
    // need the collection of docs, this is mock
    val categories = immutable.Seq.empty
    val categoryProbs = categories.map(c => (c, log(probC(c)) + logPDC(document, c)))
    categoryProbs.maxBy(_._2)._1
  }

  // end naive bayes


  def main(args: Array[String]) {
    val document = new Document("src/main/resources/sample.txt");
    
  }
}