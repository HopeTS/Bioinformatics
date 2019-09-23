##########################################################################
# CSC-315, Fall 2019
# Lab 3: Very hard R Lab :(
# Name: Robby Bergers
##########################################################################

library(dplyr)
library(ggplot2)
library(readr)


# 1
happiness <- tribble(~Income, ~NotTooHappy, ~PrettyHappy, ~VeryHappy, 
                     "Above Average", 26, 233, 164, 
                     "Average", 117, 473, 293, 
                     "Below Average", 172, 383, 132)
View(happiness)


# 2 (Had to save locally, link was giving 404)
survey <- read_delim("../CSC-315_survey.csv",
                     escape_double = FALSE, trim_ws = TRUE, delim=",")
View(survey)


# 3
survey.3.x <- survey$YannyOrLaurel
survey.3.y <- survey$Fight
chart.3 <- ggplot() + geom_bar(aes(x = survey.3.x, fill = survey.3.y)) + 
  labs(x = "Yanny Or Laurel", y="Number of Students", 
       fill = "Duck sized horse vs Horse sized ducks") + 
  theme_classic()
chart.3


# 4
t <- table(survey$Fight, survey$YannyOrLaurel)

# 4a
t.4a <- prop.table(t, margin = 2)
View(t.4a)
# ~86% Of Yanny chose horse-sized duck

# 4b
t.4b <- prop.table(t, margin = 1)
View(t.4b)
# 75% Of Horse-sized ducks were Yanny


# 5
t.hsGPA <- survey$hsGPA
t.collegeGPA <- survey$collegeGPA
t.5 <- ggplot()  + 
  geom_point(aes(survey$hsGPA, survey$collegeGPA)) +
  theme_classic() + 
  labs(x = "High School GPA", y = "College GPA",
       title = "High School vs. College GPA") +
plot(t.5)

ggplot(data = NULL, aes(survey$hsGPA, survey$collegeGPA)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "High School GPA", y = "College GPA",
       title = "High School vs. College GPA") +
  geom_smooth(method = "lm", color = "darkred")


# 6
plot.cor <-function(x,y, location = "topleft", ...) {
  plot(x,y, ...)
  abline(h = mean(y), col = "red")
  abline(v = mean(x), col = "red")
  l = lm(y~x)
  abline(l, col = "blue")
  r = cor(x,y)
  r = paste("r = ", round(r,2))
  legend(x = location,r)
}
plot.cor(survey$hsGPA, survey$collegeGPA)
# There is a positive correlation between high school GPA and college GPA


# 7



# 8
View(mtcars)


# 9


# 10
