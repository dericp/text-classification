package com.gmail.pderichai.text.classification

/**
  * Main class to run any of the algorithms
  */
object Main {

  /**
    * main function to run text classification
    * args should consist of:
    *   a field denoting the algorithm ("nb" -> naive bayes, "lr" -> logistic regression, "svm" -> svm)
    *   a field denoting the file set on which to run the algorithm ("validate" or "test")
    * @param args
    */
  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("You have not entered the correct number of arguments!")
      return 0
    }

    (args(0), args(1)) match {
      case ("nb", "test") => NaiveBayesMain.runOnTestSet()
      case ("lr", "test") => //LogisticRegressionMain.test()
      case ("svm", "test") => //SVMMain.test()
      case ("nb", "validate") => NaiveBayesMain.runOnValidationSet()
      case ("lr", "validate") => 2
      case ("svm", "validate") => 3
    }

  }

}
