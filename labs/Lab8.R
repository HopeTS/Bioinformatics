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
################################################################

GSE1297 <- ReadAffy(celfile.path = "/Users/dancikg/Downloads/")

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



###########################################################
# Perform a two sample t-test to evaluate whether or not
# expression is significantly different between CONTROL
# patients and patients with SEVERE AD. Report the 
# fold change and the p-value and state your conclusion.
###########################################################




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

#####################################################
## Repeat the boxplot, t.test, scatterplot, and 
## cor.test for the gene PSEN1 using the probe 
## 207782_s_at
#####################################################


########################################################
## Based on the above analyses, what is your conclusion
## about the association between the genes APOE and
## PSEN1 and Alzheimer's Disease / cognitive 
## impairment?
###############################################s######

###########################################################
## If you are interested, more information about
## Alzheimer's Disease and these genes can be found
## at: http://ghr.nlm.nih.gov/condition/alzheimer-disease
##########################################################
