###################################
## writing functions in R
###################################

## example with two arguments ##
divide <-function(x,y) {
    return(x/y)
}
divide(3,4)

## example with a default argument ##
divide <-function(x,y = 1) {
  return(x/y)
}

### arguments are matched in order unless they are named
divide(3)         # 3 / 1 (since 1 is default y-value)
divide(3,2)       # 3/2 = 1.5
divide(2,3)       # 2/3 = .666
divide(y = 2, 3)  # 3/2 = 1.5

###################################################
# return is implicit if the function ends with
# an expression (or an assignment)
##################################################

# same divide function as above, with implicit return
divide <-function(x,y = 1) {
  x/y
}

divide(3,2) # 3/2 = 1.5

#################################################
## for loops - for each element in a 
##        vector or list
#################################################

# basic format: iterate over each element in a vector (or list) 
xvalues <- c(1,3,2,9)

# loop over each value of x
for (x in xvalues) {
    cat (x, " ")
}

# 'traditional approach' to loop over each index value
for (i in 1:length(xvalues)) {
  cat (xvalues[i], " ")
}


######################################################################
## Note: In most cases, functional approaches (apply, lapply, sapply)
##    are preferred over 'for loops'. Functional approaches are
##    simpler and more readable (once understood). 
##
##    In R, a 'functional' is a function that takes another function
##    as an argument
#####################################################################

######################################################################
## apply - applies a function to a row (MARGIN = 1) 
## or column (MARGIN = 2) of a matrix. 
######################################################################

###########################################
# example: find max of each row of 'm'
###########################################
m <- matrix(1:20, ncol=5, byrow=TRUE)
m
rowMaxes <- apply(m, 1, max)
rowMaxes

###############################################################
## add the 'n' smallest numbers in a vector
## Note: could also return sum(sort(x)[1:n])
###############################################################
add.smallest <- function(x, n=2) {
  x.sorted <- sort(x)
  sum(x.sorted[1:n])
}
ans.row <- apply(m, 1, add.smallest) ## for each row
ans.row

ans.col <- apply(m, 2, add.smallest) ## for each col
ans.col

# additional arguments can be included after the function name
ans.row.4 <- apply(m, 1, add.smallest, n = 4)
ans.row.4

# function to add the 2 smallest values of a vector
add2Smallest <- function(x) {
  sum(sort(x)[1:2])
}

# use apply with named function, applied to each row
apply(m, 1, add2Smallest)

## alternative format using an inline (anonymous) function
apply(m, 1, function(x) {
    sum(sort(x)[1:2])
})

## another alternative format using an inline (anonymous) function
apply(m, 1, function(x) sum(sort(x)[1:2]))

## lapply applies a function to each object in a list, and returns a list
person <- list(name = "Bob", sibling.ages = c(43,21), pet.ages = c(8,3))
lapply(person, length)

## sapply does the same but returns a vector
sapply(person, length)

########################################
# if statements - follows C/C++/Java format
########################################
compare <-function(x, ref = 0) {
  if (x < ref) {
    cat("the number", x, "is less than", ref, "\n")
  } else if (x > ref) {
    cat("the number", x, "is greater than", ref, "\n")
  } else {
    cat("the number", x, "is equal to", ref, "\n")
  }
}

compare(5, 3)

#######################################################################
# Question set (use the grades matrix below):
#######################################################################

grades <- matrix(c(71,86,82,93,87,92,85,85,98,99,100,92),ncol=3,byrow=T)
rownames(grades) <- c("Steve", "Joe", "Jane", "Andrea")

# 1. Using the apply function, find the following:

# (a) mean grade for each student (rows)

# (b) mean grade for each assignment (columns)


# 2. Write a function called 'is.A' that takes a vector, and returns TRUE 
#    if the mean value of the vector is >= 90 (in the A range). Then use
#    this function and 'apply' to identify the names of
#    the students with an A average. Can you write code that outputs 
#    only the names of the students with As?

