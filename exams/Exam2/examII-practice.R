# CSC 315, Exam II Practice Problems

# Note: This is not a comprehensive review, but contains exercises covering some
# of concepts that may appear on Exam II. In addition to these exercises,
# make sure you understand concepts covered in lecture and on the previous labs.

# Directions: Modify this script to add R code in order to answer the questions 
# and/or complete the steps below. 


# 1. Blackjack time! A player has a blackjack if dealt two cards with a point
#    value of 21. The cards contain an Ace (worth 11 points),
#    and a 10 or a face card (J, Q, or K), worth 10 points. We will use
#    the number 1 to denote an Ace and 11,12, and 13 for the face cards.
#    

# A deck of cards is given by the following code:

deck <- rep(1:13,4)

# The code below deals two cards and calculates the point value 
hand <- sample(deck, 2) #draw 2 hands from the deck
hand[hand > 10] <- 10  # face cards are worth 10 points
hand[hand == 1] <- 11  # aces are worth 11 points
sum(hand) # find the point total


# (a). Find the probability of being dealt a blackjack as follows:  
#    write a function, using the code above, that randomly draws 
#    a 2-card hand from a deck, and returns TRUE if the hand is a 
#    blackjack (i.e., if the point value is 21). 
#    Then simulate 5000 blackjack games and find the empirical probability 
#    that the player is dealt a blackjack. 

# Blackjack function
blackjack <- function() {
  hand <- sample(deck, 2) #draw 2 hands from the deck
  hand[hand > 10] <- 10 
  hand[hand == 1] <- 11  
  if (sum(hand) == 21) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}

# Blackjack Trials
blackjack.true <- 0
blackjack.false <- 0
blackjack.trials <- c(1:5000)
for(i in blackjack.trials) {
  blackjack.trial <- blackjack()
  if (blackjack.trial == TRUE) {
    blackjack.true <- blackjack.true + 1
  } else {
    blackjack.false <- blackjack.false + 1
  }
}

# Empirical probability
blackjack.ep <- blackjack.true / 5000
blackjack.ep


# (b). Find the theoretical / classical probability of being a dealt a blackjack, by
#      answering the questions below. The code below generates the sample space for 
#      all 2-card hands
library(gtools)
hands <- combinations(52, 2, deck, repeats.allowed = FALSE, set = FALSE)
hands[hands > 10] <- 10 
hands[hands == 1] <- 11  

#   (i) How many possible 2-card hands are there?
sum(hands)

#   (ii) Find the probability that a player is dealt a blackjack

# Set up
blackjack.hands <- 0
not.blackjack.hands <- 0
check.blackjack <- function(x) {
  if (sum(x[1], x[2]) == 21) {
    sum(x[1], x[2])
    return (TRUE)
  } else {
    return (FALSE)
  }
}

# Get data
blackjack.prob <- apply(hands, 1, check.blackjack)
for (val in blackjack.prob) {
  if (val == TRUE) {
    blackjack.hands <- blackjack.hands + 1
  } else {
    not.blackjack.hands <- not.blackjack.hands + 1
  }
}

# Find probability
blackjack.hands / sum(blackjack.hands, not.blackjack.hands)


#2. In 2013, the proportion of adults who smoke in the U.S. was 0.18. 
#   A 2015 study involving 1000 adults found that 163 of them smoked.
#   Is there evidence that the smoking rate has changed?

# (a) State the null and alternative hypotheses

#h0: The smoking rate in adults has not changed significantly from 2013 to 2015
#ha: The smoking rate in adults has changed significantly from 2013 to 2015

# (b) Calculate / find the test statistic
btest <- prop.test(163, 1000, p = 0.18)

# (c) Find the p-value 
btest$p.value

# (d) State the conclusion regarding the null and alternative hypotheses in 
#     the context of this problem.

#Null hypothesis confirmed, alternative hypothesis refuted

#3. Consider a null hypothesis about a population proportion or comparing two 
#   population proportions, and the following Z test statistics. Find the p-value, 
#   and state whether you would REJECT the null hypothesis, or FAIL TO REJECT
#   the null hypothesis.

# (a) Z = -1.76
pnorm(-1.76)
2*pnorm(-abs(-1.76))
#p value > 0.05, therefore FAIL TO REJECT null hypothesis

# (b) Z = 2.9

# (c) Z = 0 (additional question: why would Z be equal to 0)? 



#4. Consider a null hypothesis about a single population mean 
#   and the following t test statistics and sample sizes. Find the p-value, and
#   state whether you would REJECT the null hypothesis, or FAIL TO REJECT
#   the null hypothesis.

# (a) t = -3.11, n = 23
n = 23
a4 <- 2*pt(-abs(-3.11), df=n-1)
a4
#p < 0.05, therefore null hypothesis is REJECTED

# (b) t = .29, n = 97
n = 97
b4 <- 2*pt(-abs(0.29), df=n-1)
b4
#p > 0.05, therefore FAIL TO REJECT null hypothesis

# (c) t = -1.3, n = 348
n = 348

b4 <- 2*pt(-abs(-1.3), df=n-1)
b4
#p > 0.05, therefore FAIL TO REJECT null hypothesis

# For questions (5) - (6), state the following:

# (a) The null and alternative hypotheses (these must include parameters 
#     such as p and mu, and definitions for these parameters)


# (b) What would it mean in the context of this problem if a Type I error occured?

# (c) What would it mean in the context of this problem if a Type II error occured?


#5. A study is conducted to determine whether the proportion of females in
#   the United States differs from 50%.

#a.
#h0: P is equal to 0.5
#ha: P is not equal to 0.5

#b.
#type 1: Concluding P is not equal to 0.5 when it is

#c.
#type 2: Concluding P is equal to 0.5 when it is not


#6. A study is conducted to determine whether or not the average age of an adult
#   male in the U.S. is different than the average age of an adult female.

#a.
#h0: MU M - MU F = 0
#ha: MU M - MU F != 0

#b.
#type 1: Concluding MU M - MU F != 0 when it = 0

#c.
#type 2: Concluding MU M - MU F = 0 when it != 0


#7. A study is conducted to compare two drugs that reduce flu symptoms in a group of
#   patients. Suppose that for drug #1, 43 / 100 individuals have reduced flu symptoms,
#   and for drug #2, 37 / 111 individuals have reduced flu symptoms. 

# i) State the null and alternative hypotheses
#h0: p1 - p2 = 0
#ha: p1 - p2 != 0

# ii)  Find the test statistic
drug1 <- c(43, 100)
drug2 <- c(37, 111)
btest <- prop.test(43, 100, p = (37/111))
btest

# iii) Find the p-value 
btest$p.value

# iv) State the conclusion. Is there evidence that one drug is more effective than the other?
# We can conclude that both drugs have similar results
    
