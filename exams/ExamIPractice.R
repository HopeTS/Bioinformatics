# CSC 315, Exam I Practice Problems

library(dplyr)
library(ggplot2)
library(reshape2)
library(readr)

# Note: This is not a comprehensive review, but contains exercises
# covering some of the concepts that will be on the first exam. 
# In addition to these exercises, make sure you understand concepts 
# covered in lecture and on the previous labs.

# Directions: Modify this script to add R code in order to answer the questions 
# and/or complete the steps below. 

# Note: For the exam, you will submit a Notebook containing code and answers 
# to the problems below. All graphs must be created in ggplot unless
# stated otherwise, and be given appropriate titles and axis labels.


# 1. Create a vector called ages that contains the numbers 21, 24, 32, and 19

ages <- c(21, 24, 32, 19)

# 2. Create a vector called evens that contains all even numbers 
#     between 1 and 100, and the number 200.
evens <- c(seq(2, 100, 2), 200)
evens

# 3. Write a function called min.positive which takes a vector 'x' as an
#    argument and returns the the smallest positive number from the vector.
#    For example, for v <- c(-3,10,2), min.positive(v) would return 2

min.positive <- function(x) {
  min(x[x>=0])
}
ex <- c(2, -6, -9, 15, 4)
min.positive(ex)


# 4. Write a function called min.max which takes a vector x as an argument 
#    and returns a list containing two named elements, the minimum of x and the
#    maximum of x. Use this function to find the minimum and maximum of the 
#    vector ages from problem (1). 
min.max <- function(x) {
  c(min(x),max(x))
}
min.max(ages)


# 5. Include the following code in your script to create a matrix filled with 
#    5 columns and 20 rows, that is filled with random numbers between 0 and 1.

      m = matrix(runif(100), ncol = 5, nrow = 20)
      ran.m <- matrix(sample(0:1, 100, replace=T), ncol = 5, nrow = 20)
      ran.m

#   (a) Find the median of each row 
      apply(ran.m, 1, median)
#   (b) Find the median of each column.
      apply(ran.m, 2, median)

# 6. Run the command below to read in an old class survey:
      
      survey <- read.delim("http://pastebin.com/raw/1csmBawE")
      
# 7. The code below generates a scatterplot of College GPA against Alcohol
#    consumption that includes the regression line, and colors the points by
#    Gender.
      
      ggplot(survey, aes(Alcohol, College.GPA)) + 
        geom_point(aes(color = Gender)) + 
        geom_smooth(method = 'lm', color = 'black', se = FALSE) +
        theme_classic() + ggtitle('Alcohol Consumption vs. College GPA') +
        xlab('Alcohol consumption (days / week)')

    
    # (a) Fit a linear model that predicts college GPA from Alcohol consumption
    #     (i)  Find and interpret the y-intercept
      fit <- lm(survey$College.GPA ~ survey$Alcohol)
      fit  # Intercept: 3.3267
      
    #     (ii) Find and interpret the slope
      plot(fit)
      abline(survey$College.GPA, survey$Alcohol)
      
        
# 9. For those who agree with same sex marriage legalization, find the mean 
#    and standard deviation of College GPA.
      testing <- survey$Marijuana.Legalization == 'Agree'
      survey.9 <- filter(survey, Marijuana.Legalization == 'Agree')
      mean(survey.9$College.GPA)    # ~ 3
      sd(survey.9$College.GPA)    # 0.45
      
# 10. Construct side-by-side boxplots for FB usage based on whether or not
#    a person agrees or disagrees with same sex marriage 
#    Was there an association between FB usage and views on same sex marriage
#    in this class?
      
      ggplot(m) + geom_col(aes(type, value, fill = presence)) +
        labs(y = "Proportion", fill = "Pesticide status", 
             title = "Distribution of pesticide status by food type") +
        theme_classic()
      
      ## display a side-by-side barchart, by changing the position argument 
      ## to 'dodge' in geom_bar
      ggplot(m) + geom_col(aes(type, value, fill = presence), position = "dodge") +
        labs(y = "Proportion", fill = "Pesticide status", 
             title = "Distribution of pesticide status by food type") +
        theme_classic()
      
  
# 11.  Construct a stacked bar graph showing with one bar for males and one for 
#      females, and each bar showing the proportion who agree and disagree
#      with marijuana legalization. Was there a relationship between gender
#      and views on marijuana legalization in this class?

    