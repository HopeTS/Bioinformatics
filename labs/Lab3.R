library(dplyr)
library(ggplot2)
library(readr)


# 1
happiness <- tribble(~Income, ~NotTooHappy, ~PrettyHappy, ~VeryHappy, "Above Average", 26, 233, 164, "Average", 117, 473, 293, "Below Average", 172, 383, 132)
View(happiness)


# 2
