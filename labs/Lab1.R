##########################################################################
# CSC-315, Fall 2019
# Lab 1: R programming
# Name: Robby Bergers
##########################################################################

##########################################################################
# Add R code to the script below in order to explicitly output the 
# answers to the following questions. Turn in this assignment
# by creating a Notebook and turning in a physical copy.
##########################################################################

#1) What is 34+29*12
34 + 29 * 12

#2) What is the sum of all integers between (and including) -100 and +100.
sum(-100:100)

#3)	Create a vector that contains the numbers 1-10.
vector <- c(1:10)

#4) Change the 3rd value of the above vector to 99
vector[3] <- 99

#5)	Create a vector that contains the numbers 1-10, 17, and 25.
vector.2 <- c(1:10,17,25)

#6)	Create a vector that contains two elements, your first name and last name
vector.3 <- c('Robby', 'Bergers')

#7) Create a matrix that contains the numbers 87, 89, and 91 in the 1st row and
#   76, 88, and 83 in the second row. Set or change the column names to 
#   "ExamI", "ExamII", and "ExamIII", and set or change the row names to 
#   "Joseph Smith" and "Amy Davis"
matrix.1 <- matrix(
  c("ExamI", 87, "ExamII", 89, "ExamIII", 91),
  nrow=2,
  ncol=3
)
rownames(matrix.1) <- paste(list("Joseph Smith", "Amy Davis"))

#8) Calculate the average grade for Amy Davis, using the 'mean' function.
mean(as.numeric(matrix.1[2,]))

#9) "Joseph" prefers to be called "Joe". Change the name of the 1st row of the matrix 
#   to "Joe Smith". You should do this in a statement that only changes the name of 
#   the first row. Note that R allows you to directly assign a value to any element 
#   (or elements) of rownames(m).
rownames(matrix.1) <- list("Joe Smith", "Amy Davis")
print(matrix.1)

#10)	Create a list that includes the following objects: 
#       (1) a vector that contains two elements, your first name and last name; 
#       (2) your major
robby.major <- "Computer Science"
robby.name <- c("Robby", "Bergers")
robby.list <- list(robby.name, robby.major)

#11)  Read in the survey.txt file as was done in class (and put the code for this in your script!)
library(readr)
survey <- read_delim("https://gdancik.github.io/CSC-315/data/datasets/survey.txt",
                     escape_double = FALSE, trim_ws = TRUE, delim="\t")
View(survey)

#12)	How many individuals surveyed did not use FB (i.e., spent 0 hours / week on FB)

#13) What are the GPAs of the three students with the lowest College GPAs (you 
#    should only display these GPAs)? Hint: use the 'sort' function. 

#14) What are the GPAs of the three students with the highest college GPAs? 
#    Hint: use the sort function with decreasing = TRUE

#15) Use the 'filter' function from 'dplyr' to create a data frame (tibble) 
#    called surveyA that includes the students with a 3.5 college GPA or higher

#16) Display the 'Gender' column from surveyA and comment on the number of 
#    individuals who are male, and the number who are female.
