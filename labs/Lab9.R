##############################################################
# Name: Robby Bergers
# CSC-315
# Lab #9: Limma, heatmaps, and analyzing processed GEO data
#############################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps below and to explicitly answer the following questions
##########################################################################

library(limma)
library(GEOquery)
library(ggplot2)
library(dplyr)
library(affy)

##################################################################################
# 1.The code below reads in our class survey data and performs a 
#   2-sample t-test to evaluate whether there is a statistically
#   significant difference in Hours of Sleep between 'Cat' vs. 'Dog'
#   people. Based on the code below, (a) find the p-value and state 
#   your conclusion regarding the null hypothesis of H0: mu_cat - mu_dog = 0;
#   and (b) calculate the difference in mean Alcohol consumption between
#   groups, using the formula: 
#     mean hours of sleep for dog people - mean hours of sleep for cat people
##################################################################################
  survey <- read.csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")
  s <- split(survey$Sleep, survey$CatOrDogPerson)
  res <- t.test(s$Cat, s$Dog, var.equal = TRUE)

  res

# s2 <- split(survey$Alcohol, survey$CatOrDogPerson)

  s.cat <- mean(s$Cat)
  s.dog <- mean(s$Dog)
  s.sleep <- s.dog - s.cat
  s.sleep
  
  print('A p value of 0.79 Shows there isn\'t a statistically significant difference between mean hours of sleep for cat and dog people')

##################################################################################
# 2.Fit a linear model that predicts Hours of Sleep based on 
#   whether an individual is a cat or a dog person. You should use
#   the treatment contrast where 'cat person' is the reference (x = 0) and 
#   'dog person' is the treatment (x = +1)
#    
# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.'

  dog.person <- as.integer(survey$CatOrDogPerson == "Dog")
  a.2 <- lm(survey$Sleep ~ dog.person, data = survey)
  a.2

  print('The y intercept is 6.3 hrs of sleep.')  

# (b) Find and interpret the slope of the regression line in the context of 
#     this problem
  
  print('The slope of the regression line is 0.2')

# (c) What is the p-value for the hypothesis test that there is a
#     significant difference in Hours of Sleep between the two groups?
#     (show this result in R, based on the linear model) Note: the p-value 
#     from the linear model should match the p-value from the two-sample 
#     t-test from problem 1(a) above.
##################################################################################
  
  summary(a.2)
  print('The p value of 0.79 shows there is no statistically significant difference')

###############################################################
# 3. Get the processed data for GSE19143 and pull out the 
#    expression data and phenotype data. Note that this
#    dataset contains gene expression samples from children
#    with Acute Lymphoblastic Leukemia (ALL), a cancer of
#    the bone marrow. Tumor samples were treated with
#    the anti-inflammatory drug prednisolone, and determined 
#    to be either sensitive (responsive) or resistant 
#    (non-responsive) to this drug. 
###############################################################

  GSE19143 <- getGEO("GSE19143")
# GSE19143.rma <- rma(GSE19143)
  GSE19143.affy <- ReadAffy(celfile.path = "c:/Users/berge/Documents/GitHub/Bioinformatics/labs/GSM_DATA_2/")
#  GSE19143.rma <- rma(GSE19143)
  GSE19143.expr <- exprs(GSE19143[[1]])
  GSE19143.expr <- log2(GSE19143.expr)
  GSE19143.p <- pData(GSE19143[[1]])
  
#  View(GSE19143.expr)

# (a) How many samples had their gene expression values profiled?
  
  ncol(GSE19143.expr)

# (b) How many probes are on the array?
  
  nrow(GSE19143.expr)

# (c) Take the log2 of the expression data, and generate a boxplot
#     to show that the samples are properly processed and normalized.
#     The analysis beginning with question 5 must use the log2 data; 
#     otherwise the results will not be correct.

  c.3 <- data.frame(GSE19143.expr, rownames(GSE19143.expr))
  boxplot(GSE19143.expr)
  
  
#####################################################################
# 4.How many individuals are resistant to prednisolone and
# how many are sensitive? 
#####################################################################

  prednisolone <- levels(GSE19143.p$characteristics_ch1.5) <- c("Resistant", "Sensitive")
  prednisolone <- GSE19143.p$characteristics_ch1.5
  table(prednisolone)
  print('25 are resistant, 27 are sensitive')
  
#####################################################################
# 5. Find the top differentially expressed probes, with a FDR of 10%,
# between individuals that are resistant vs. sensitive to prednisolone.
# Note: there should be 16 probes total. How many of these probes 
# are up-regulated (i.e., have higher expression) in resistant 
# individuals and how many are down-regulated (i.e., have lower 
# expression) in resistant individuals. 
#####################################################################
  
  matrix <- model.matrix(~0+prednisolone)
  colnames(matrix) <- c("Resistant", "Sensitive")
  fit <- lmFit(GSE19143.expr, matrix)
  matrix.2 <- makeContrasts(Resistant - Sensitive, levels=matrix)
  fit.2 <- contrasts.fit(fit, matrix.2)
  fit.2 <- eBayes(fit)
  fdr <- topTable(fit.2, p.value = 0.10, number = 16)

########################################################################
# 6. Construct a heatmap of these top 16 probes, with individuals 
# color-coded by response to prednisolone (with green=sensitive and 
# red = resistant). (Note: if you are unable to complete question 5), 
# you may do this with the first 16 probes in the expression matrix).
########################################################################

  m <- match(rownames(fdr), rownames(GSE19143.expr))
  m.2 <- GSE19143.expr[m,]
  col.heat <- colorRampPalette(c("red", "green"))(200)
  plot(1:200, col = col.heat, pch = 18, cex = 3)
  col.resistance <- as.integer(as.factor(prednisolone))
  col.resistance <- c("red", "green")[col.resistance]
  heatmap(m.2, ColSideColors = col.resistance, col = col.heat)
  
########################################################################
# 7. If you answered question 5 correctly, the SECOND hit 
# should be for the probe 209374_s_at. Show that this probe
# corresponds to the gene IGHM, by first downloading the 
# correct platform data from GEO, and then finding the gene
# associated with this probe. 
#######################################################################

  platform <- annotation(GSE19143[[1]])
#  platform
  pl <- getGEO(platform)
  pl <- Table(pl)
  m <- match("209374_s_at", pl$ID)
  pl$`Gene Symbol`[m]

#####################################################################
# 8. How many probes are there for the gene IGHM on the platform
# in this study? Note: you must search for this gene using the
# regular expressions covered in the GEO-and-limma.R script. Your 
# code must also output the number of probes.
####################################################################

  probe <- grep("IGHM", pl$`Gene Symbol`)
  df <- data.frame(pl[probe,])
  nrow(df)
  print('There are 34 probes')

########################################################################
# Final Notes: the heatmap in question 6 provides a candidate list
# of probes associated with prednisolone response in children with 
# leukemia. Although much additional work and testing would need to be 
# done, this kind of gene signature could ultimately be used to 
# determine whether a child with leukemia would benefit from 
# prednisolone treatment, or whether an alternative treatment might be 
# more effective.

# The IGHM finding is also interesting. IGHM is a gene that codes
# for an antibody protein involved in the immune reponse; the 
# fact that this gene is differentially expressed beween responders and 
# non-respnoders suggests that a patient's immune system may play a
# role in how they respond to prednisolone)
########################################################################