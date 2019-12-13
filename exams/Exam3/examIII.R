# CSC 315, Exam II 
# Name:   Robby Bergers              [2 points]        

# Directions: Modify this script by adding R code to answer the questions and/or 
# complete the steps below. When you are finished, create a Notebook and submit 
# your Exam through the Assignments link on Blackboard 
# (https://easternct.blackboard.com/). If you are unable to submit a Notebook, 
# you may submit the R code instead (but with a point deduction)
# Notebook is worth 5 points. All questions are worth 5 points each except for
# questions 4 and 7 (10 points each)

library(dplyr)
library(ggplot2)
library(limma)
library(affy)

##################################################################################
# 1.The code below reads in our class survey data.
#   Fit a linear model that predicts Alcohol consumption based on 
#   whether an individual is a cat or a dog person. You should use
#   the treatment contrast where 'cat person' is the reference (x = 0) 
#   and 'dog person' is the treatment (x = +1). Then answer the questions
#   below. 
#    

survey <- read.csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")

# creates plot, code cat person as 0 and Dog person as +1
ggplot(survey, aes(as.integer(survey$CatOrDogPerson == "Dog"), survey$Alcohol)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE) + theme_classic() +
  xlab("Cat (x = 0) or Dog (x = 1) person") + ylab("Alcohol consumption (days per week)") +
  ggtitle("Alcohol consumption / week for Cat and Dog people")


# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.  

x <- lm(survey$CatOrDogPerson == "Dog" ~ survey$Alcohol)
summary(x) 
# intercept: 0.75, slope: -0.1 

# (b) Find and interpret the slope of the regression line in the context of 
#     this problem 
print("The slope of -0.1 indicates that cat people tend to drink heavier than dog people.")

# (c) What is the p-value for the hypothesis test that there is a
#     significant difference in Alcohol consumption between the two groups?
#     (show this result in R, based on the linear model) 

#p value: 0.107


# (d) Verify the p-value from (c) is identical to the p-value obtained from
#     the two-sample t-test with equal variance
res <- t.test(survey$CatOrDogPerson == "Dog", survey$Alcohol)
res$p.value

##################################################################################


# 2.The dataset GSE54839 on the Gene Expression Omnibus (GEO) contains 
#   gene expression profiles of brain cells from individuals who were
#   addicted to cocaine and individuals who were not (controls).
#   The command below loads in the data, which creates the following objects:

#     GSE54839.expr - expression matrix with probes in rows and
#                     samples in columns; expression is on the log
#     GSE54839.p - clinical information for each sample

load(url("http://bioinformatics.easternct.edu/BCBET2/GSE54839.RData"))

#   Answer the following questions based on the expression data:

#    (a) How many samples are there? 
ncol(GSE54839.expr) #60

#    (b) How many probes are profiled?
nrow(GSE54839.expr) #48761
#    (c) What is the mean expression value of the 3rd probe?
mean(GSE54839.expr[,3:3]) #7.8

#    (d) Find the probe with the highest mean expression
max = ""
max.num = 0
x <- apply(GSE54839.expr, 1, function(x) {
  if (mean(x) > max.num) {
    max = x
    max.num = mean(x)
  }
})
print(max(x))

# 3. In the phenotype data table, characteristics_ch1 indicates whether 
#    the individual was addicted to cocaine or not. Output the proportion
#    of individuals who are addicted to cocaine, and the proportion who
#    are not.

total <- length(GSE54839.p$characteristics_ch1) #60
addicted <- GSE54839.p$characteristics_ch1 == "disease state: cocaine addiction"
addicted <- length(addicted[addicted == TRUE])
addicted / total

# 4.  Is the probe ILMN_1897746 differentially expressed between individuals who 
#     are addicted to cocaine and those who are not? Construct a boxplot 
#     (using ggplot) of the expression of this probe across the two groups,
#     and report the fold-change and p-value. 
GSE54839.probe = match("ILMN_1897746", rownames(GSE54839.expr))

GSE54839.probe

# 5.  The code below uses limma to find differentially expressed probes
#     between those who are and are not addicted to cocaine. Run the
#     code and then answer the questions that follow:

library(limma)

# construct design matrix
design <- model.matrix(~0+GSE54839.p$characteristics_ch1)

# let's change the column names
colnames(design) <- c("addict", "control")

## limma package fits a linear model to each row of the expression matrix ##
fit <- lmFit(GSE54839.expr, design)

## Specify the contrasts, which must match column names of design matrix ##
contrast.matrix <- makeContrasts(addict - control,levels=design)

## fit model based on contrasts (e.g., Female - Male)
fit2 <- contrasts.fit(fit, contrast.matrix)

# calculate moderate t-statistics by moderating standard errors
# toward a common value, which makes answers more robust
fit2 <- eBayes(fit2)

# Find differentially expressed probes using an FDR of 1%
tt <- topTable(fit2,p.value = 0.01, sort.by = "p", number = nrow(GSE54839.expr))

# (a) Use R to output the number of probes that are differentially expressed
#     at a false discovery rate (FDR) of 1%?
nrow(tt) #24

# (b) Output the probe name, the log fold change, and the adjusted p-value 
#     (and only these columns) for the top 5 probes. (Note: you may create
#     a new data frame to do this, though it is not necessary).
top.expr <- tt$AveExpr >= 9.8
top.tt <- tt[tt$AveExpr >= 9.8, ]
print(rownames(top.tt))
print(top.tt$logFC)
print(top.tt$P.Value)

# 6. The platform data can be loaded by running the command below, which will
#    load the object 'pl' that contains the platform information. Note that
#    the gene is found in the column 'ILMN_Gene' and the probe is in the 
#    'ID' column

load(url("http://bioinformatics.easternct.edu/BCBET2/GPL6947.RData"))

#     (a) How many probes are on this array? 

nrow(pl)#49576

#     (b) Use R to find the gene associated with the top probe ILMN_1774077?
ilmn <- pl[pl$ID == "ILMN_1774077",]
ilmn$Symbol

#     (c) Using R, find the probes for the gene TCP1.

tcp1 <- pl[pl$Symbol == "TCP1",]
tcp1$ID


#7. Using k-nearest neighbor classification with k = 3, and the functions and
#   probes below, find the balanced accuracy using leave-one-out cross-validation 
#   for classifying cocaine addicts.

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
  mean(acc)
}

# differentially expressed probes
probes <- c("ILMN_1774077", "ILMN_2103919", "ILMN_1695706", "ILMN_1667825", 
            "ILMN_1886493", "ILMN_1803988", "ILMN_1811933", "ILMN_2233539", 
            "ILMN_1760280", "ILMN_1711092", "ILMN_1801616", "ILMN_1801766", 
            "ILMN_2144426", "ILMN_1730945", "ILMN_2307903", "ILMN_1791576", 
            "ILMN_1699265", "ILMN_1687266", "ILMN_2227573", "ILMN_1667857", 
            "ILMN_2121068", "ILMN_1756982", "ILMN_1897746", "ILMN_1720048")


library(class)


# 8. The top differentially expressed genes are provided below. Use DAVID 
#    (https://david.ncifcrf.gov/) to carry out a functional analysis and
#    submit a screenshot showing the 3 KEGG pathways associated with 
#    this set of genes. 

genes <- c("GBP2", "LRFN3", "H3F3B", "MLKL", "HS.535028", "MCL1", "SHMT1", 
    "SLC39A8", "NXT1", "KCNB1", "EMP1", "CCDC109B", "HIST2H2AA3", 
    "C19ORF4", "VCAM1", "CHSY1", "TNFRSF10B", "TMEM191B", "GSTO1", 
    "C12ORF52", "ADAM17", "CLIC1", "HS.562318", "CCL2")

write.table(genes, row.names = FALSE, col.names = FALSE, quote = FALSE)


# Extra Credit

# 1.Create a function called 'my_knn' that takes 3 inputs: a value of 'k', 
#   a (scaled) expression matrix, and the class labels. The
#   function returns the balanced accuracy from a leave-one-out cross-
#   validation for the given inputs.

# 2. Use 'sapply' to calculate the balanced accuracy for odd k-values between
#    1 and 21; 

# 3. Use 'ggplot' to plot construct a scatterplot with 'k' values on the x-axis
#    and the balanced accuracy on the y-axis. What is the optimal value of 'k'?


