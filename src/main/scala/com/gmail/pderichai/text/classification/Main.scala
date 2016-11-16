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
    val instructions = "Re-run with arguments for algorithm and doc set. See README for more details."

    if (args.length == 0) {
      println(instructions)
      return 0
    }

    if (args.length != 2) {
      println("You have not entered the correct number of arguments!")
      return 0
    }

    (args(0), args(1)) match {
      case ("nb", "test") => NaiveBayesMain.runOnTestSet()
      case ("lr", "test") => LogisticRegressionMain.runOnTestSet()
      case ("svm", "test") => SVMMain.runOnTestSet()
      case ("nb", "validate") => NaiveBayesMain.runOnValidationSet()
      case ("lr", "validate") => LogisticRegressionMain.runOnValidationSet()
      case ("svm", "validate") => SVMMain.runOnValidationSet()
    }

  }

}
