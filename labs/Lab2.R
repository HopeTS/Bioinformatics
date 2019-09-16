########################################################
# Name: Robby Bergers
# CSC-315
# Lab #2: Graphical and Numerical Summaries of Data
########################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions.
# Your Notebook include output showing the requested tables and
# graphs, and answers to questions should be provided in comments.
# Also, all graphs must be given an appropriate title, x-axis label, and
# y-axis label. The ggplot2 library must be used to generate all
# graphs unless stated otherwise.
##########################################################################

# 1.load our classes survey data 
#   (available at https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv)
#   and add the code for this to the script. 

library(readr)
survey <- read_delim("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv", ",")
View(survey)

# 2. How many students completed the survey?

nrow(survey) # = 17 => 17 students

# 3. How many questions were asked (i.e., how many columns are there)?

ncol(survey) # = 9 => 9 questions

# 4. Construct a frequency table for the response to whether someone is a
#    'Cat' or 'Dog' person.

survey.table <- table(survey$CatOrDogPerson)
prop.table(survey.table)
View(survey.table)

# 5. Construct a frequency bar graph for the response to "Are you a cat or a dog person?",
#    where the bars are colored using the default colors. Remove the legend by 
#    adding the following component to the end of your 
#    ggplot() code: theme(legend.position = "none")

library(ggplot2)
survey.cod <- survey$CatOrDogPerson
survey.cod.t <- table(survey.cod)
prop.table(survey.cod.t)
survey.cod.df = data.frame(survey.cod = survey.cod)

ggplot(survey.cod.df, aes(x=survey.cod)) + geom_bar(aes(fill=survey.cod)) + 
  ggtitle("Cats or Dogs??") + 
  labs(x = "Cat or Dog", y = "Frequency") + theme(legend.position = "none")

# 6. Construct a relative frequency table for favorite CSC course. 
#     What proportion of students said that CSC-340 was their 
#     favorite? Note that the two missing values, denoted with the 
#     keyword NA, are correctly ignored by R when the tables are 
#     constructed using what was covered in class.

survey.6 <- survey$FavoriteCourse
survey.6.t <- prop.table(table(survey.6))
View(survey.6)

# 7. Construct a Pareto Chart using the frequencies for favorite CSC course (you may display
#    either frequency or relative frequency). Note: you may do this using
#    either 'geom_bar' with the raw data or 'geom_col' to work directly
#    with the frequencies or relative frequencies. However, if you use
#    geom_bar, then the missing values will be included in the graph. 
#    If you want to remove the missing values, you can use the 'drop_na'
#    function from the 'tidyverse' package. What course or courses were most
#    commonly listed as the favorite?



# 8. Construct a relative frequency table for whether or not a student consumes alcohol
#    at least 1 day per week, on average (i.e., consumes alcohol > 0 days per week).
#    Do this by first creating a logical vector where TRUE corresponds to consuming
#    alcohol and FALSE corresponds to does not consume alcohol. Then create a relative
#    frequency of these TRUE and FALSE values. Tables and relative frequency tables
#    are stored as named vectors (e.g., x <- c(item1 = 1, item2 = 2)). Use the 'names' 
#    function to change the names from FALSE and TRUE to "Consumes alcohol" and
#    "Does not consume alcohol"

survey.alcohol <- survey$Alcohol
survey.alcohol.rate <- survey.alcohol > 0
survey.alcohol.rate <- c(survey.alcohol, names(survey.alcohol.rate) <- c ("Consumes alcohol", "Does not consume alcohol"))
prop.table(table(survey.alcohol.rate))


# 9. Out of the people who heard "Laurel" in this class, would they rather fight one 
#    horse-sized duck or one hundred duck-sized horses? Answer this question by using
#    dplyr's 'filter' function to create a new data.frame for those who heard "Laurel". 
#    Then generate a relative frequency table for the 'Fight' column results. 
#    Repeat the analysis to answer the same question for those who heard "Yanny"
#    What do you conclude about a person's choice regarding the "Fight" question?

library(dplyr)
survey.laurel <- filter(survey, survey$YannyOrLaurel == "Laurel")
survey.laurel.fight <- survey.laurel$Fight
prop.table(table(survey.laurel.fight))

# 10. Construct a histogram for Alcohol consumption, by using the hist() function with the argument
#     breaks = 14 to set the number of groupings. Describe the shape of its distribution. 
#     Is it unimodal, bimodal, or flat. Is it skewed right, skewed left, or symmetric?

survey.hist <- hist(survey$Alcohol, breaks=14)
print("I don't understand this kind of math, but it looks unimodal and skewed right")

# 11. Calculate the mean and median for Alcohol consumption. 
#     Which is a better measure of averages? (Note: although these numbers are similar,
#     one would still be considered better than the other -- why?)

the.mean <- mean(survey$Alcohol)
the.median <- median(survey$Alcohol)

# 12. What is the 75th percentile for HS GPA??

print(quantile(survey$hsGPA, .75))

# 13. Ten percent of indivduals have HS GPAs above what value?

print(quantile(survey$hsGPA, .9))

# 14. Create side-by-side boxplots showing the average hours of sleep 
#     based on a person's gender, and answer the questions below:
#     (a) Does there appear to be a difference in the 'median' amount of 
#         sleep between those who identify as 'Female' compared to 'Male'.
#     (b) What does the difference in the boxes indicate?
#         groups? 
#     (c) Are there any outliers? If so, how many?

# 15. Adding a 'facet_grid' creates multiple plots by
#     splitting up your data based on one or more 
#     variables. Use ggplot to construct side-by-side 
#     boxplots comparing hsGPA with gender, then add
#               + facet_grid(rows = vars(YannyOrLaurel))
#     in order to generate two sets of boxplots, one for
#     the people who heard 'Yanny' and one for the 
#     people who heard 'Laurel'. What is the relationship
#     between hsGPA between males and females, and how
#     does this relationship differ between the 'Laurel' 
#     and 'Yanny' groups?


# 16. For college GPA, what is the variance and standard deviation?

survey.16.var <- var(survey$collegeGPA)
survey.16.sd <- sd(survey$collegeGPA)

# 17. Create a vector with 20 values that has a standard deviation of 0.

x <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# I'm inept don't judge me
sd(x)

