
###########################
#                         #
#   Team Assignment 2     #
#                         #
###########################

## Please submit one set of answers per team.            ##
## Your answers may be submitted as an annotated R file. ##
###########################################################


#################
## Question 1: ##
#################

# For this problem you will use the data in the file "teamassign02data01.csv" to implement   
# a simple form of the bootstrap resampling method.  Repeat (a) and (b) 1000 times:
#
#   (a) From the data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- which is OK and expected.
#   (b) Use your sample to generate a regression equation. Save the values of 
#       hat(beta_0) and hat(beta_1).
#   (c) Find and report a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the confidence 
#       intervals contain the true parameter values?


#################
## Question 2: ##
#################
library(tidyverse)

# Import the data set "teamassign02data02.csv" which contains 100 sets of data for 
# the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
data2 <- read_csv('teamassign02data02.csv')
#
#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2) and pair up 
#       the y-values with corresponding rows from the data set of x-values.
set.seed(1)
randy <- rnorm(100, mean=10, sd = 5)
data2$y <- randy
#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values as explanatory variables.
mlreg.model <- lm(y ~ ., data2)
#   (c) Determine the number of significant explanatory variables at the 5% level.
# summary(mlreg.model)
# names(summary(mlreg.model)$coefficients[,4])[summary(mlreg.model)$coefficients[,4] <= .05] # This will return the full list of parameter estimates significant at the 5% level
names(summary(mlreg.model)$coefficients[,4][-1])[summary(mlreg.model)$coefficients[,4][-1] <= .05] %>%
  length()
# OUTPUT: 2
#   (d) Determine and report the proportion of significant variables in the 100
#       simulations. Compare this proportion with the expected theoretical value.
num_simulations <- 100
num_significant <- sapply(X = 1:num_simulations, function(X, data2){
  # (a)
  randy <- rnorm(100, mean=10, sd = 5)
  data2$y <- randy
  # (b)
  mlreg.model <- lm(y ~ ., data2)
  # (c)
  num_significant <- names(summary(mlreg.model)$coefficients[,4][-1])[summary(mlreg.model)$coefficients[,4][-1] <= .05] %>%
    length()
}, data2 = data2)
# The following is the proportion of significant variables
sum(num_significant) / (num_simulations * (NCOL(data2) - 1)) # Make sure to remove the B_0 column
# OUTPUT: 4.65%
# We should expect only 5% of all variables to be significant considering that we generated y to be completely random and unrelated to x.
# So at a 5% significance level, we'd expect only 5% of variables to be significant just by chance