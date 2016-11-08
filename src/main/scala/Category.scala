

class Category {
  var termFrequencies = scala.collection.mutable.Map.empty[String, Int];
  var documents = scala.collection.mutable.Seq.empty[Document];
  
  def incrementTF(term: String, incr: Int) {
    termFrequencies(term) = termFrequencies.getOrElse(term, 0) + incr;
  }
}