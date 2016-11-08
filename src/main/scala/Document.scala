import scala.io.Source

class Document (val filePath: String) {
  // , var prevCategories: scala.collection.mutable.Map[String, Category]
  var categories = scala.collection.mutable.Seq.empty[String];

  //i think this should be done outside so we don't store this entire document in the object -i
  private val document = Source.fromFile(filePath).mkString.split("[ .,;:?!\t\n\r\f]+")
  var termFrequencies = document.groupBy(identity).mapValues(_.length);
  var length = termFrequencies.map(_._2).sum;
  
  println("length " + length)
  println("termFrequencies " + termFrequencies)
  
  ////// SKIPPED CATEGORY ASSIGNMENTS - see library
  // Constructor
  
  private def parseDoc() {
    // add term frequencies in categories
    // for each category
    val c = new Category();
    termFrequencies.foreach{ case (term, count) => c.incrementTF(term, count) }
    // c.documents.append(self);  WHAT IS 'THIS'
  }
}