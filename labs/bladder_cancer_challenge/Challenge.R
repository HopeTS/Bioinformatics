library(limma)
library(class)

rm(list = ls()) ## this will delete all objects in the workspace

###################################################################
# Useful functions
###################################################################

## scale rows of given matrix to have mean 0 and sd of 1
row.scale <-function(x) {
  x.scale = t(scale(t(x)))
  return(x.scale)
}

# returns the balanced accuracy
balanced.accuracy <-function(predicted, true) {
  t <- table(true = true, predicted = predicted)
  if (nrow(t) != 2) {
    stop("invalid number of rows in accuracy table")
  }
  acc <- diag(t) / rowSums(t)
  sens1 <- acc[1]
  sens2 <- acc[2]
  list(table = t, sensitivity1 = sens1, sensitivity2 = sens2, balanced.accuracy = mean(acc))
}


###################################################################
# Load the Challenge Data
###################################################################

# X.train - the log2 gene expression data for the training samples
# Y.train - the class labels (NMI or MI) for the training samples
# X.test - the log2 gene expression data for the test samples

load(url("https://gdancik.github.io/CSC-315/data/hw/Challenge.RData"))

# 1) Find differentially expressed probes in your training dataset

# 2) Using the differentially expressed probes, evaluate a knn classifier 
#    using leave-one-out cross-validation in the training data set. 
#    Don't forget to scale your data (training and testing). 
#    Find the balanced accuracy.

# 3) Classify the test samples, and email your predictions to
#    dancikg@easternct.edu with the subject: Bioinformatics Challenge. In the
#    e-mail, include your team name (be creative!), and team member names,
#    followed by the predictions, with 1 prediction per line. Note: to get your
#    predictions in text format, with one prediction per line, and with no other
#    text, use the write.table function with row.names = FALSE

# 4) Optimize at least one of the parameters using a classification method of
#    your choice, based on the balanced accuracy from leave-one-out cross-validation.
#    You are encouraged to explore other classifiers in addition to kNN.

# 5) Once you have optimized your classifier, classify the test samples and e-mail
#    me your predictions following the directions in (3).

# 6) Submit a Notebook following the instructions in the PDF.
