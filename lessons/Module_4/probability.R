######################################
## Module 4: Probability
######################################

# Note: the 'gtools' library is needed to enumerate
# permutations and combinations

library(gtools)
library(ggplot2)
library(dplyr)

######################################################
# simulate rolling a die 10000 times, by sampling
# from the values 1:6, 10000 times, with replacement
######################################################
roll.num <- sample(1:6, 10000, replace = TRUE)

## treat the results as a factor (qualitative variable) and construct a barplot
roll <- factor(roll.num)
ggplot() + geom_bar(aes(roll, fill = roll)) + 
  ggtitle("Outcome from rolling die 10000 times") +
  theme(legend.position = "none")


# function to find the proportion of sixes occuring in first 
# 'i' elements of 'x' 
proportion.sixes <- function(i,x) {
  count <- sum(x[1:i]==6)
  count/i
}

## apply this function to all integers from 1- 10000
props <- sapply(1:length(roll.num), proportion.sixes, x=roll.num)


## plot empirical and theoretical probabilities
six.plot <- ggplot() + geom_point(aes(x = 1:length(props), y=props), color = "blue") +
  ggtitle("Empirical probability of rolling a 6 with a fair die") +
  labs(x = "# rolls", y = "proportion of sixes rolled") +
  geom_hline(aes(yintercept=1/6, linetype = "theoretical\nprobability"),color = "red") +
  theme_linedraw() +
  scale_linetype_manual(name = "", values = 2) # this adds the legend (values = 2 for dotted line)


# plot only first 100 rolls
six.plot + xlim(0,100)

# look at complete plot
six.plot                


## simulate flipping a fair coin 1000 times
coins <- sample(c("H", "T"), 1000, replace=TRUE)

# generate bar graph of frequencies; the guides() function is
# used to suppress the legend for the 'fill' elements (the coins)
ggplot() + geom_bar(aes(coins, fill = coins)) + 
  ggtitle("Outcome of flipping a fair coin 1000 times") +
  labs(x = "outcome", y = "Frequency") +
  guides(fill = FALSE)
  

# calculate relative frequency table, then 
# generate bar graph of relative frequencies 

# create data.frame containing relative frequency table
p <- table(coins) %>% prop.table() %>% data.frame()  

# plot relative frequencies; the guides() function is
# used to suppress the legend for the 'fill' elements (the coins)
ggplot(df) + geom_bar(aes(x = coins, y = Freq, fill = coins), stat = "identity") + 
  ggtitle("Outcome of flipping a fair coin 1000 times") +
  labs(x = "outcome", y = "Relative Frequency") +
  geom_hline(aes(yintercept=1/2, linetype = 'theoretical probability'),
             color = "black") +
  scale_linetype_manual(name = "", values = 2) +
  guides(fill = FALSE) + ylim(0,1) + theme_classic()


# Let's repeat this simulation using a biased coin, flipped 1000 times
# (the biased coin has a 90% probability of Heads, 10% probability of tails)
coins <- sample(c("H", "T"), 1000, prob = c(.9,.1), replace=TRUE)

# create data frame of relative frequency table
p <- table(coins) %>% prop.table() %>% data.frame()



ggplot(df) + geom_bar(aes(x = coins, y = Freq, fill = coins), stat = "identity") + 
  ggtitle("Outcome of flipping a biased coin 1000 times") +
  labs(x = "outcome", y = "Relative Frequency") +
  geom_hline(aes(yintercept=9/10, linetype = "theoretical probability"),color = "black") +
  scale_linetype_manual(name = "", values = 2) +
  guides(fill = FALSE) + ylim(0,1)


############################################################
## general method for repeating a probability experiment --
## write a function to do the experiment once, then
## use the replicate function to repeat the experiment
############################################################

## example function: flip a fair coin 2 times, 
#     determine if we get 2 heads
# returns TRUE if we get 2 heads, FALSE otherwise
flip.two.heads <- function() {
  f <- sample(c("H", "T"), 2, replace = TRUE)
  count <- sum(f=="H")
  count == 2
}

# flip a coin 2 times, repeat 1000 times
two.heads <- replicate(1000, flip.two.heads())

# find the empirical probability of getting 2 heads = 
# number of times we get 2 heads / number of experiments
prop.heads <- sum(two.heads) / length(two.heads)
prop.heads


#########################################################
# Exercise: Copy and modify the above code to find
# the empirical probability of flipping a coin 3 times
# and getting at least 2 heads.
#########################################################


#########################################################
## classical probability - when all outcomes
## are equally likely, 
## P(A) = (number of outcomes in A) / 
##          (number of outcomes in sample space)
#########################################################

##########################################################
# We can use the 'permutation' function from the gtools
# library for classical probability calculations
# the arguments for the permuation function include 
# (1) number of outcomes per trial, 
# (2) number of trials,
# (3) sample space for each trial
# (4) repeats = TRUE if outcomes can repeat across trials
# Permutations gives all possible arrangements where
#    order matters
##########################################################

##########################################################
# Find the sample space S for flipping a coin 3 times  #
##########################################################
S <- permutations(2,3, c("H", "T"), repeats = TRUE)

# Define the event corresponding to getting exactly two heads
index <- rowSums(S == "H") == 2
S[index,]

# Define the event corresponding to getting at least two heads
index <- rowSums(S == "H") >= 2
S[index,]

#######################################################
# Finding classical probabilities
#######################################################

##########################################################
## aside: useful logical functions: 
#   any(x) returns TRUE if any element in x is TRUE
#   all(x) returns TRUE if all elements in x are TRUE
##########################################################

x <- 1:3
any(x == 4)
any(x == 1)
all(x == 1)

x <- c(1,1,1)
all(x == 1)

# Find the probability of getting all heads, P(H = 3), when
# flipping a fair coin 3 times
all.heads <- apply(S == "H", 1, all)
sum(all.heads) / length(all.heads)

# Find the probability of getting at least 1 Head, P(H >= 1)
any.heads <- apply(S == "H", 1, any)
p.any.heads <- sum(any.heads) / length(any.heads)
p.any.heads

# Note that the complement of at least one head (H >=1)
# is no heads (H=0) (or all tails). Therefore by the 
# rule of complements, P(no heads) = 1 - P(at least 1 heads)

# probability of no heads
1 - p.any.heads

# probability of no heads - direct calculation
no.heads <- apply(S == "T", 1, all)
p.no.heads <- sum(no.heads) / length(no.heads)
p.no.heads
