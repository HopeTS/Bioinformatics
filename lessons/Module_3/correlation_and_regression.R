#########################################
# Correlation and regression
#########################################

library(ggplot2)

#######################################
## load internet data
#######################################

internet <- read.delim("http://pastebin.com/raw/enxWu6R6")

fb.penetration <- internet$Facebook.Penetration..Percent.
internet.penetration <- internet$Internet.Penetration..Percent.

##############################################
# what is the mean value of internet and
# fb penetration?
##############################################

##############################################
# describe the shape of the distributions for
# internet penetration.
##############################################

##################################################
# scatterplots: plots a point for each pair of 
#   (x,y) values
##################################################
ggplot()  + geom_point(aes(internet.penetration, fb.penetration)) +
              theme_classic() + 
              labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
                title = "Internet and FB penetration rates")


## where is the United States?
colors <- rep("black", length(internet.penetration))
colors[internet$Country == "USA"] = "red"

## replot with colors to highlight the United States
ggplot() + 
  geom_point(aes(internet.penetration, fb.penetration), 
             color = colors) +
  theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates")

ggplot() + 
  geom_point(aes(internet.penetration, fb.penetration), 
             color = colors) +
  theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates") 


########################################################################
## Is there a trend in the data?
## what countries are potential outliers (from the trend in the data)
########################################################################

## Let's highlight the outlier
## Note: for element-by-element comparison across vectors, use 
#  single & or | (and NOT && or ||)
index = fb.penetration < 10 & internet.penetration>60
internet$Country[index]

#######################################################################
# Note: the code below is used to generate datasets with various
# correlations; you do not need to understand the code, but you do 
# need to understand how correlation is interpreted
#######################################################################

#######################################################################
# example correlations
#######################################################################
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


#######################################################################
# generate data
#######################################################################
x <- rnorm(100)
y1 <- rnorm(100, mean = x, sd = .4)
y2 <- rnorm(100, mean = -x, sd = .4)
y3 <- rnorm(100, mean = x, sd = 2)
y4 <- rnorm(100, mean = -x, sd = 2)
y5 <- rnorm(100)

#######################################################################
# plot data along with correlations
#######################################################################
par(mfrow = c(3,2))
par(mar = c(2,2,2,2)+.1)
plot.cor(x,y1)
plot.cor(x,y2, location = "topright")
plot.cor(x,y3)
plot.cor(x,y4, location = "topright")
plot.cor(x,y5)
plot.cor(x, x**2)


### reset to 1 panel for plotting (not necessary on console)
par(mfrow = c(1,1))

#############################################
# find correlation betweeen internet and FB
# penetration
#############################################
ggplot() + 
  geom_point(aes(internet.penetration, fb.penetration)) +
  theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates")


## to get correlation ##
cor(internet.penetration, fb.penetration)

#############################################
# linear regression
#############################################

###################################################################
## fit a linear regression line with internet penetration as the
## explanatory or independent variable (x) and FB penetration as
## the response variable (y)
###################################################################

###############################################################################
## Questions: Find and interpret the y-intercept of the linear regression line
##            Find and interpret the slope
################################################################################# get predicted values for each observation ##

fit = lm(fb.penetration ~ internet.penetration)

## display results ##
fit

## display summary of results (more information) ##
summary(fit)

# the y-intercept is: 3.09, which means when internet penetration is 0%,
# the predicted FB penetration rate would be 3.09%

# the slope is 0.4602, which means as internet penetration goes up by 1%,
# fb penetration goes up by 0.4602%.

## add regression line to current plot ##
ggplot(data = NULL, aes(internet.penetration, fb.penetration)) + 
  geom_point() +
  theme_classic() + 
  labs(x = "Internet Penetration (%)", y = "FB Penetration (%)",
       title = "Internet and FB penetration rates") +
  geom_smooth(method = "lm", color = "darkred")
  


########################################################################
# Use predict(fit, df) to make predictions with the fitted model
#      fit - a fitted linear model, obtained from 'lm'
#      df - a data frame with column names the same as those used to
#            fit the original model
########################################################################

# if df is not specified, predictions are based on the original data
predict(fit)

## what is the predicted FB penetration rate for a country that
## has 50% internet penetration
predict(fit, data.frame(internet.penetration=50))

## what is the predicted FB penetration rate for countries that
## have 50% and 80% internet penetrations
predict(fit, data.frame(internet.penetration=c(50,80)))

# CAUTION: what if we mess up and don't use correct variable names?
# (R gives you an incorrect answer -- you may get a warning but will not get an error!)
predict(fit, data.frame(x=50))


#################################################################
# A linear model gives the equation of the line that minimizes
# the sum of the squared residuals. The code below illustrates
# this minimization
#################################################################

## generate a scatterplot and visualize residuals for lm(y~x) ##
plot.resids <- function(x,y, ...) {
  plot(x,y, pch = 19,...)
  l = lm(y~x)
  abline(l, col = "red")
  points(x, predict(l), pch = 19, col = "red")
  segments(x,predict(l), x, y, col = "blue")
  legend("topleft", c("obs", "prediction", "residual"), 
         lty = c(0,0,1), pch = c(19,19,-1),col = c("black", "red", "blue"))
}


plot.resids(internet.penetration, fb.penetration)


############################################################################
## Usually we fit a linear model using data from a data frame, using
## the format below
############################################################################

internet.table <- data.frame(internet = internet.penetration, fb = fb.penetration)
fit <- lm(internet ~ fb, data = internet.table)  # data must be specified
predict(fit, data.frame(fb = .5))


############################################################################
## CAUTION: you will not be able to make predictions if you fit a model using, e.g.
##      fit <- lm(internet.table$internet ~ internet.table$fb) because the 
##         explanatory variable is then 'internet.table$fb' which cannot be 
##         specified in the predict function
############################################################################

######################################################
# Find the regression for the ANNUAL temp against YEAR 
# and complete the questions below
######################################################
temps <- read.delim("http://pastebin.com/raw/KZgkViBK")
tempPlot <- ggplot(temps, aes(YEAR, ANNUAL)) +
            geom_point() + 
            geom_smooth(method = "lm", color = "darkgreen", 
                        fullrange = TRUE)

tempPlot

fit <- lm(ANNUAL ~ YEAR, data = temps)

# 1. Find and interpret the slope

# 2. Find and interpret the y-int

# 3. Predict average temp in the year 1999

# 4. Predict average temp in the year 3000
