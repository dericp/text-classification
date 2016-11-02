import ch.ethz.dal.tinyir.io.ReutersRCVStream

object HelloWorld {

  def main(args: Array[String]): Unit = {
    // ReutersRCVSStream requires absolute path...lol
    val reuters = new ReutersRCVStream("/Users/pderichai/workspace/text-classification/src/main/resources")
    val docStream = reuters.stream
    val doc = docStream.
  }

}