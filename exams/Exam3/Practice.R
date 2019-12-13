# CSC 315, Exam III Practice Problems

# Note: This is not a comprehensive review, but contains exercises covering some
# of the concepts that will appear on Exam III. In addition to these exercises,
# make sure you understand concepts covered in lecture and on the previous labs 
# and project.

# Directions: Modify this script to add R code in order to answer the questions 
# and/or complete the steps below. 

library(ggplot2)

# 1.Fit a linear model that predicts Petal Width based on the type of flower from
# the iris dataset (see below). Code your explanatory variable 
# using x = 0 if the sample is of type "versicolor" and x = +1 if the sample is of type 
# "virginica" (type "setosa" will be ignored). 
#    
# (a) Find and interpret the y-intercept of the regression line in the
#      context of this problem.
# (b) Find and interpret the slope in the context of this problem
# (c) Based on your linear model, what is the p-value for the hypothesis test that there is a
#     significant difference in Petal Width between the two flower types? 
#     Note that you can extract the exact p-value from the coefficients object
#     (which is a matrix) of the fitted linear model. 
# (d) Verify the p-value from (c) is identical to the p-value obtained from
#     the two-sample t-test with equal variance

#  The code below constructs a scatterplot of this data (the iris data is available 
#  by default). Note that you will need to remove or ignore "setosa" in your analysis.

ggplot(iris, aes(Petal.Length, Petal.Width, color = Species)) + 
  geom_point() + theme_classic() + 
  theme(legend.background = element_rect(color = "black",
                                         linetype = "solid")) 

# 2.The dataset GSE29561 on the Gene Expression Omnibus (GEO) contains 
#   gene expression profiles of breast cancer tumors treated by chemotherapy
#   (epirubicin and docetaxel). Read in the processed data for GSE29561, 
#   and extract the expression data and phenotype data. Note: this data
#   is already on the log2 scale. 
#   Answer the following questions based on the expression data:
#    (a) How many samples are there? 
#    (b) How many probes are profiled?
#    (c) What is the mean expression value of the 3rd probe?


# 3. In the phenotype data table, characteristics_ch1.1 indicates whether 
#    the sample responded to the therapy (treatment response = 1) or 
#    not (treatment response = 0).  What proportion of samples responded 
#    to the therapy?



# 4.  Is the probe 202271_at differentially expressed between samples 
#     that responded to therapy and samples that did not? 
#     Construct a boxplot (using ggplot) of the expression of this probe 
#     across the two groups, and report the fold-change and p-value. 

# 5. Download the appropriate platform data.
#     (a) How many probes are on this array? 
#     (b) What gene is associated with the probe 202271_at?


# 6.  Using the limma package, find the top differentially expressed probe
#     (sorted by p-value). What is the probe name, the fold-change, 
#     and the adjusted p-value. Is there evidence that this probe is 
#     differentially expressed? Why or why not? (Note: limma requires that the 
#     column names of your design matrix NOT be numbers)


# 7. Based on the probes below, generate a heatmap and color the columns with "darkgreen" for
#    "treatment resonse in vivo: 1" and "darkred" for "treatment response in vivo: 0". In 
#    other words, resistant individuals are dark red and sensitive individuals are dark green

probes = c("201690_s_at","212611_at","218721_s_at",
           "219463_at","205014_at","216092_s_at",
           "207375_s_at","202271_at","221551_x_at","219053_s_at")


#8. Using k-nearest neighbor classification with k = 3, find the balanced accuracy 
#   from a leave-one-out cross-validation for response = 0 vs. response = 1, using the 
#   probes from problem (7).