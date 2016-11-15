# text-classification

Move the zipped training, validation, and test data into src/main/resources/validation/, src/main/resources/training/,
and src/main/resources/test/. Place the src folder and the build.sbt into the same folder and run "sbt compile".

Using SBT run, pass the proper arguments into the main method.

In order to run our classifiers, run Main.scala with the following runtime arguments:

The first argument corresponds to which algorithm to use for the classifier:

    - "nb" --> use Naive Bayes
    - "lr" --> use logistic regression
    - "svm" --> use SVM

The second argument corresponds to which data set to run the algorithm on:

    - "test" --> run on test set and create the text output file mapping doc IDs to categories
    - "validate" --> run on validation set and print out a final F1 score based on the results

Examples of running the program:

    >  nb test
        to return text file "ir-2016-project-13-nb.txt" for running naive bayes on test set
    >  svm validate
        to print out an F1 score for the SVM classifier based on the validation set

** output file for test set gets returned to top-level project folder **
** all arguments are case sensitive **