################################################################
# Lab 8: GEO Lab - Raw Data
# In this lab you will analyze two probes from a gene expression
# study of Alzheimer's Disease (AD). The dataset is
# available from: 
# http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE1297
################################################################

library(affy)
library(dplyr)
library(ggplot2)

#####################################################################
# Download the raw data (GSE1297_RAW.tar file) and extract the files  
# Read in the data using theReadAffy() function and the 
# celfile.path argument which must be set appropriately below
#################################################################

GSE1297 <- ReadAffy(celfile.path = "c:/Users/berge/Documents/GitHub/Bioinformatics/labs/GSM_DATA/")

################################################################
# Process the gene expression data using the Robust Multi-Array 
# Average (RMA) method and extract the expression data.
# How many probes and samples does this dataset contain?
# Generate a boxplot of the expression values of each sample
# to confirm that the data has been normalized
################################################################

###################################################################
# We will see how to get the phenotype data from GEO in a later
# class; for this lab, the data has already been processed
# and can be read in using the statement below. The data includes
# MMSE.Score = mini–mental state examination score for 
#   cognitive impairment (low scores indicate impairment)
# NFT.Score = protein markers for AD
################################################################

GSE1297.p <- read.delim("http://bioinformatics.easternct.edu/BCBET2/GSE1297.p.xlsx")

##################################################################
# The code below gets the group names from the sample names of the 
# pheno table and constructs a scatterplot of MMSE and NFT
# scores with points color-coded by AD severity.  
# (note: code assumes that the pheno table is stored in GSE1297.p)
###################################################################

##################################################
# get group names from sample names, which have
# format "Group SampleNumber"
##################################################
sample.names <- as.character(GSE1297.p$Sample)
groups <- gsub(" .*", "", sample.names)

# update the phenotype data with the group names
GSE1297.p <- mutate(GSE1297.p, AD.status = groups)

GSE1297.rma = rma(GSE1297)
GSE1297.expr = exprs(GSE1297.rma)

ggplot(GSE1297.p, aes(MMSE.Score, NFT.Score)) +
  geom_point(aes(color = AD.status), size = 3) + 
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_classic() +
  ggtitle("Relationship between MMSE score, NFT score, and AD severity") +
  theme(legend.box.background = element_rect(color = "black")) +
  scale_color_manual(values = c("darkblue", "orange", "purple", "red"))
  
########################################################
# Describe the relationship between MMSE and NFT score.
# Would you expect a person with a high MMSE score to
# have Alzheimer's Disease?
#####################################################

print("On average, a higher NFT score indicates a lower MMSE score. 
      The higher the MMSE score, the less likely the patient has Alzheimer's.")

#####################################################
# A gene called APOE is associated with late onset
# Alzheimer's disease. One of the probes for 
# APOE is 203381_s_at
#####################################################

############################################################
# Construct side-by-side boxplots showing the expression
# of the probe 203381_s_at for CONTROL patients and 
# patients with SEVERE AD. (The boxplot must be constructed
# using ggplot -- see notes for an example)
############################################################

probe.203381sat = match("203381_s_at", rownames(GSE1297.expr))

df = data.frame(expr = GSE1297.expr[probe.203381sat,], type = GSE1297.p$AD.status)

ggplot(df,aes(type, expr, fill = type)) + geom_boxplot() +
  theme_classic() + theme(legend.position = "none") + 
  labs(x = "AD Severity", y = "Log2 Expression") +
  ggtitle("Expression vs. AD Severity")

###########################################################
# Perform a two sample t-test to evaluate whether or not
# expression is significantly different between CONTROL
# patients and patients with SEVERE AD. Report the 
# fold change and the p-value and state your conclusion.
###########################################################

severity.test <- t.test(df$type == "Severe", df$type == "Control")

split <- split(df$expr, df$type, drop = TRUE)

lapply <- lapply(split, mean)
#lapply$Control - lapply$Severe
log.foldc <- lapply$Control - lapply$Severe
fc <- 2**log.foldc

severity.test$p.value
fc
print('P value: 0.56, Fold change: 0.89')
print('The P value shows that there is a difference in expression between control and severe group')

#####################################################
# Construct a scatterplot of gene expression of
# the probe 203381_s_at on the x-axis and MMSE score
# on the y-axis, coloring the points by AD status as
# was done for the above scatterplot. Give the graph 
# an appropriate title, axis labels, and legend, 
# and also add the regression line, as was done above. 
# What is the correlation between MMSE score and 
# expression? 
#####################################################

probe.match <- match("203381_s_at", rownames(GSE1297.expr))

df <- data.frame(expr = GSE1297.expr[probe.match,], MMSE = GSE1297.p$MMSE.Score,
                type = GSE1297.p$AD.status)

ggplot(df, aes(expr, MMSE)) +
  geom_point(aes(color = type), size = 3) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_classic() +
  labs(x = "203381_s_at Expression", y = "MMSE Score")
  ggtitle("203381_s_at Expression vs. AD Severity") +
  theme(legend.box.background = element_rect(color = "black")) +
  scale_color_manual(values = c("darkblue", "orange", "purple", "red"))

##########################################################
# The cor.test function can be used to evaluate the
# following hypotheses:

# H0: r = 0, where r is the correlation between x and y
# HA: r != 0

# The function is called using cor.test(x,y), where x 
# and y are the vectors of observations. Find the p-value, 
# report the correlation, and state whether or not the 
# correlation between expression and MMSE score is 
# statistically significant 
##########################################################

  cor.test(df$expr, df$MMSE)
  print('P value: 0.56')
  print('The high P value shows that there is a statistically significant relationship between expression and MMSE score')
  
#####################################################
## Repeat the boxplot, t.test, scatterplot, and 
## cor.test for the gene PSEN1 using the probe 
## 207782_s_at
#####################################################

  probe.match.2 = match("207782_s_at", rownames(GSE1297.expr))
  df.2 = data.frame(expr = GSE1297.expr[probe.match.2,], type = GSE1297.p$AD.status)
  
  ggplot(df.2,aes(type, expr, fill = type)) + geom_boxplot() +
    theme_classic() + theme(legend.position = "none") + 
    labs(x = "AD Severity", y = "Log2 Expression") +
    ggtitle("Expression vs. AD Severity")

  severity.test.2 = t.test(df.2$type == "Severe", df.2$type == "Control")
  split.2 <- split(df.2$expr, df.2$type, drop = TRUE)
  lapply.2 <- lapply(split.2, mean)
  log.foldc.2 <- lapply.2$Control - lapply.2$Severe
  fc.2 <- 2**log.foldc.2
  
  severity.test.2
  fc.2
  
  print('P value: 0.57, Fold change: 0.63')
  print('The P value shows there is a statistically significant difference for 207782_s_at')
  
  df.3 <- data.frame(expr = GSE1297.expr[probe.match.2,], MMSE = GSE1297.p$MMSE.Score,
                   type = GSE1297.p$AD.status)
  
  ggplot(df.3, aes(expr, MMSE)) +
    geom_point(aes(color = type), size = 3) +
    geom_smooth(method = "lm", color = "black", se = FALSE) +
    theme_classic() +
    labs(x = "207782_s_at Expression", y = "MMSE Score")
  ggtitle("207782_s_at Expression vs. AD Severity") +
    theme(legend.box.background = element_rect(color = "black")) +
    scale_color_manual(values = c("darkblue", "orange", "purple", "red"))
  
  cor.test(df.3$expr, df.3$MMSE)
  
  print('P value: 0.01')
  print('The P value shows that there is no statistically significant difference between 207782_s_at and MMSE')

########################################################
## Based on the above analyses, what is your conclusion
## about the association between the genes APOE and
## PSEN1 and Alzheimer's Disease / cognitive 
## impairment?
######################################################

###########################################################
## If you are interested, more information about
## Alzheimer's Disease and these genes can be found
## at: http://ghr.nlm.nih.gov/condition/alzheimer-disease
##########################################################
