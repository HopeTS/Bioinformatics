###############################################################
# k-nearest neighbor (KNN) example
###############################################################

library(GEOquery)
library(limma)
library(ggplot2)

###############################################################
# Let's load in the GSE1297 data again.
###############################################################

GSE1297 <- getGEO("GSE1297")

###########################################################
# Pull out gene expression data and pheno type data. Note
# that for this dataset, expression data must be logged -- 
# see GEO-and-limma.R
###########################################################

GSE1297.expr <- exprs(GSE1297[[1]])
GSE1297.expr <- log2(GSE1297.expr)
GSE1297.p <- pData(GSE1297[[1]])

################################################################
# Find the differentially expressed genes between males and
# females
################################################################

# change levels to F vs M to simplify output later on
levels(GSE1297.p$characteristics_ch1.6) <- c("F", "M")

gender <- as.character(GSE1297.p$characteristics_ch1.6)
design <- model.matrix(~0+gender)
colnames(design) <- c("Female", "Male")

## fit linear model to each row (probe) of expression matrix
fit <- lmFit(GSE1297.expr, design)

## specify the contrasts
contrast.matrix <- makeContrasts(Female - Male,levels=design)

## fit model based on contrasts (e.g., Female - Male)
fit2 <- contrasts.fit(fit, contrast.matrix)

# calculate moderated t-statistics by moderating standard errors
# toward a common value, which makes answers more robust
fit2 = eBayes(fit2)

## get all probes with FDR < 0.05, sorted by p-value 
tt.05 <- topTable(fit2,sort.by = "p", p.value = 0.05, number = nrow(GSE1297.expr))

##############################################################################
# In classification problems, it is often desirable to scale each probe,
# so that no single probe has a dominating effect. Below is an example of
# why we do this
##############################################################################

##############################################################################
# function to print the Euclidian distance matrix of x and to 
# plot the cluster
#############################################################################
plot.clust <-function(x) {
  d <- dist(t(x))
  print(d)
  h <- hclust(d)
  plot(h)
}

## generate data
r <- c(1.5, 2, 3) #each row
M <- rbind(p1=r,p2=r,p3=r,p4=r) # 4 rows
colnames(M) <- c("A","B","C")
M

# View clusters: in this case samples A and B are the closest
# (expected based on M)
plot.clust(M)


# Another example: in this case a probe with large expression
# is added, and samples A and C are the 'closest'
# this is not desired since 'closeness' is completely 
# determined by probe p5 
M <- rbind(M, p5=c(10,15,10))
M
plot.clust(M)

# Solution is to scale each probe (each row) so that each probe 
# has the same importance. We use the following functions:
#   scale - scales each column to have mean 0 and sd of 1
#   t - the transposte (switches rows and columns)
row.scale <-function(x) {
  x.scale <- t(scale(t(x)))
  return(x.scale)
}

# With row scaling, A and B are the most similar
# With gene expression data, it is usually desirable to scale 
# each probe. Otherwise, probes with relatively high 
# or low expression will drive the classification
M.scale <- row.scale(M) # scale each row
plot.clust(M.scale)


###############################################################
## back to microarray data 
###############################################################

## get probes that are differentially expressed
m <- match(rownames(tt.05), rownames(GSE1297.expr))
X <- GSE1297.expr[m,]

# visualization of probes - with knn, an unknown observation (?)
# is classified based on it's k-nearest neighbors. Note: only
# 2 dimensions are shown, though 12 probes (dimensions) are 
# used for classification
col.gender <- as.integer(as.factor(gender))
col.gender <- c("pink", "blue")[col.gender]
X.scale <- row.scale(X)


df <- data.frame(probe1 = X.scale[1,], probe2 = X.scale[2,], gender = gender)

ggplot(df, aes(probe1,probe2,color=gender)) + geom_point() +
  theme_classic() + ggtitle("Scaled expression of top 2 probes") +
  scale_color_manual(values = c("hotpink", "blue")) +
  annotate("text", x = -.3, y = -.5, label = "? (M or F)") 

# we will use the plotly package for a 3D plot 
# (this package will need to be installed)
library(plotly)
labels <- list(xaxis = list(title = "probe 1", range = c(-2.2, 2.2)),
               yaxis = list(title = "probe 2", range = c(-2.2, 2.2)),
               zaxis = list(title = "probe 3",range = c(-2.2, 2.2)))
plot_ly(x=X.scale[1,], y=X.scale[2,], z=X.scale[3,], 
        type="scatter3d", mode="markers", color = gender, colors=c("pink", "blue")) %>%
        layout(scene = labels)


###################################################################
# Let's predict the gender of each individual, using the 12 probes
# with FDR < 5%. Note that knn requires samples in rows and 
# features (probes) in columns
###################################################################

library(class) # required for knn

###################################################################
# Leave one out cross-validation (loocv):
# for each sample, predict its class after removing it from the 
# training set. 
# Use knn.cv(data,classes, k), where 
#   data - the data (with probes in COLUMNS and samples in ROWS)
#   classes - the known classes corresponding to the data
#   k - value of k for the 'k' nearest neighbors
###################################################################
preds = knn.cv(t(X.scale), gender, k = 3) 
table(true = gender, predicted = preds)

# overall accuracy (% correct) #
sum(preds == gender) / length(gender)

#######################################################################
# The overall accuracy is not a good measure of performance because it
# depends on how balanced the data is.
# The 'sensitivity' (or 'recall') of class 'A' is the probability of 
# correctly classifying samples from group A. The 'balanced accuracy'
# is calculated as the average sensitivity/recall over all classes.
#######################################################################

balanced.accuracy <-function(predicted, true) {
   t <- table(true = true, predicted = predicted)
   if (nrow(t) != 2) {
     stop("invalid number of rows in accuracy table")
   }
   acc <- diag(t) / rowSums(t)
   sens1 <- acc[1]
   sens2 <- acc[2]
   list(table = t, sensitivity1 = sens1, sensitivity2 = sens2, avg.sensitivity = mean(acc))
}

acc = balanced.accuracy(preds, gender)
acc


#################################################
# Let's now make a prediction for 3 new samples
#################################################
X.test <- matrix(c(-4, -1, 0, -1, 0, 0, -1, -1, -1, 5, 0, 1, 
                   3, -3, 0, 0, 0, -2, 2, 1, -2, -2, -2, 1,
                   6,  7, 11, 10,  9,  8,  8,  8,  8,  9,  6,  6), 
                 nrow = 12)

# testing data should have same scaling as training data
# the function below takes a previously scaled X matrix and
# applies its scaling to rows of the matrix X
scale.transform <- function(scaledX, X) {
  X <- t(X)
  center <- attr(scaledX, "scaled:center")
  sds <- attr(scaledX, "scaled:scale")
  t(scale(X, center, sds))
}

X.test.scale <- scale.transform(X.scale,X.test)

# Use knn(train, test, classes, k), where 
#   train - training data (probes in columns and samples in rows)
#   test - testing data (probes in columns and samples in rows)
#   classes - the known classes corresponding to the training data
#   k - value of k for the 'k' nearest neighbors
preds <- knn(t(X.scale), t(X.test.scale), gender, k = 3)
preds


########################################################################
# General classification procedure:
#   Choose a classifier, and use leave-one-out cross-validation to 
#   estimate the number of probes/genes to use (based on number, or FDR
#   cutoff), and classification parameters such as 'k' in knn. Then 
#   evaluate the classfier on an independent dataset
########################################################################
