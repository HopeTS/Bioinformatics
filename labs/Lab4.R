########################################################
# Name:
# CSC-315
# Lab #4: Probability 
########################################################
  
library(gtools)

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions
##########################################################################

# Basic Probability Questions -- Use R as a calculator and specify the
# answers below

# A standard deck of cards contains 52 cards, with 13 cards from each 
#   suit (hearts, clubs, diamonds, and spades).
 
#1. If one card is selected at random, what is the probability that 

#  (a) the card is the ace of spades?
a1 <- paste(toString((1/52)*100), '%', sep='')
print(a1)

#  (b) the card is NOT the ace of spades?
a2 <- paste(toString((51/52)*100), '%', sep='')
print(a2)

#  (c) the card is an ace (of any suit)?
a3 <- paste(toString((4/52)*100), '%', sep='')
print(a3)

#  (d) the card is an ace OR a 4?
a4 <- paste(toString((8/52)*100), '%', sep='')
print(a4)

# Use R to answer the remaining questions. You MUST use R to 
# enumerate and analyze the sample space or to carry out 
# probability experiments (simulations) in order to calculate
# an empirical probability.


#2. This question looks at the probability of rolling two dice
#   (each with values 1 - 6) and getting a sum of 7.
#   You will answer this question in parts.

#  (a) Use the 'permutations' function from 'gtools'
#      to enumerate the sample space obtained by rolling two dice.
#      (Note: the correct sample space has 36 outcomes)
    a2 <- permutations(6, 2, c(1, 2, 3, 4, 5, 6), repeats = TRUE)
    print(nrow(a2))

#  (b) Use R and your answer to (a) to find the number of outcomes 
#      where the sum is 7
fun <- function(x) {
  if (x[1] + x[2] == 7) {
    return(x)
  }
}
  b2 <- apply(a2, 1, fun)
  b2 <- Filter(Negate(is.null), b2)
  print(length(b2))

#  (c) divide your answer from (b) by the size of the sample space
#      to find the probability that the sum is 7.
  c.a2 <- length(a2) / 2
  c.b2 <- length(b2)
  print(c.a2)
  print(c.b2)
  c.a2 / c.b2
  # a2 nrow is 36, b2 nrow is 6, I don't understand how but
  # a2 length gets doubled but b2 length does not, so I have
  # to divide the length by 2


#3. Calculate the same probability in (2) but by finding the empirical 
#   probability by completing the steps below.

#   (a) Write a function that rolls two dice and returns the sum of the 
#       die rolls (this is done for you)

      roll2 <- function() {
        s <- sample(1:6, 2, replace = TRUE)
        sum(s)
      }


#   (b) Use the 'replicate' function to roll two dice 5000 times, to
#       get a vector containing the sum of die rolls for each experiment.
      nums <- c(replicate(5000, roll2()))

#   (c) Find the number of times you rolled a seven, and divide by the 
#       number of experiments to find the empirical probability
      nums.table <- table(nums)
      


# Definition: A probability distribution of a discrete random variable 
# gives the probability for each value that the variable can take. For 
# example, if we flip a coin three times, and let X = the number of heads,
# then the probability distribution of X is given by the following code:

pdist <- cbind(X = 0:3, 'P(X)' = c(0.125,0.375, 0.375, 0.125))
pdist

# In other words, P(X = 0) is 0.125, which says that the probability of 
# getting no heads (or all tails) is 0.125. You will derive the above
# probability distribution in the next problem.


#4. We will look at flipping a coin 3 times and letting X = the
#   number of heads. Find the probability distribution of X by
#   completing the steps below:

#   (a) Use the 'permutations' function to enumerate the sample
#       space obtained from flipping a coin 3 times.
    a4 <- permutations(2, 3, c("H", "T"), repeats.allowed = TRUE)
    
#   (b) Using your sample space, find X = the number of heads for each
#       set of 3 coins. 
    num.heads <- rowSums(a4 == "H") == 3
#   (c) Create a relative frequency table for X, which is the probability
#       distribution of X = the number of heads in 3 coin tosses.
    prop.table(table(num.heads))
#   (d) Create a bar graph of the relative frequencies, using ggplot and
#       labeling the x-axis, y-axis, and title. 



#5. Find the empirical distribution of X = the number of heads in 3
#   coin tosses by completing the steps below. 

#   (a) Write a function that flips a coin 3 times and returns the number
#       of heads

#   (b) Use the 'replicate' function to repeat 3 coin tosses 5000 times, to
#       get a vector containing the number of heads for each experiment


#   (c) Create a relative frequency table for the number of heads. This is
#       the empirical probability distribution of X = the number of heads
#       in 3 coin tosses.




######################################################################
#   Poker Time! The commands below enumerate the sample space of
#   all possible poker hands. Here we ignore the suit because is not 
#   needed for the questions below.  We also use combinations 
#   instead of permutations. Combinations should be used when the
#   order does not matter (e.g., which is true for the order of cards
#   in a hand). The cards are sampled WITHOUT replacement 
#   (repeats.allowed = FALSE) because we cannot include
#   the same card twice in one hand. Finally, we specify 'set = FALSE' to 
#   allow for duplicate values in the deck vector. Each combination 
#   (hand) is equally likely, so classical probability can be used.
######################################################################

deck <- rep(1:13,4)
hands <- combinations(52, 5, deck, repeats.allowed = FALSE, set = FALSE)

#6. How many possible poker hands are there?
print(length(hands))

#7. The function below takes a vector (corresponding to a hand of cards)
#   and returns TRUE if the hand contains a four-of-a-kind
#   Use 'apply' to apply this function to each hand, in order to show
#   that the probability of being dealt a four-of-a-kind is
#   approximately 0.00024 (or 1/4165). Note: You MUST use the
#   apply function and the four.of.a.kind function below to find this. 
#   Because the hands matrix contains more than 2.5 million rows, 
#   this calculation may take several minutes. You should therefore test 
#   your code on a subset of the hands matrix first. There are two 4-of-a-kinds
#   if you look at the first 20,000 rows.

##############################################################
# this function returns true if a hand contains a 4-of-a-kind
##############################################################
four.of.a.kind <- function(x) {
  t <- table(x)  # frequency table for cards in the hand
  m <- max(t)    # how frequent is the most common card?
  if (m == 4) return (TRUE)
  return (FALSE)
}


#8. Create a function that determines whether a vector 'x' contains 
#   a full house (i.e., 3 of a kind and 1 pair). You can assume
#   that 'x' includes exactly 5 cards.
matches <- 0
full.house <- function(x) {
  t <- table(x)
  m <- max(t)
  if (m == 3) {
    z <- min(t)
    if (z == 2) {
      matches <- matches + 1
      return (TRUE)
    }
  }
  return (FALSE)
}

full.house(c(9, 9, 9, 8, 8))
full.house(c(9, 9, 8, 7, 2))


#9. Show that the probability of being dealt a full house is 
#    approximately 0.00144 (roughly 1/694). Note for testing purposes,
#    that there are 18 full houses in the first 20,000 rows of the
#    hands matrix
num.matches <- apply(hands, 1, full.house)
print(sum(num.matches == TRUE))

