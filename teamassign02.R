
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

# Import the data set "teamassign02data02.csv" which contains 100 sets of data for 
# the variables x1, x2, ..., x20.  Repeat (a)-(c) 100 times: 
#
#   (a) Generate 100 y values according to the model y ~ N(10, var=5^2) and pair up 
#       the y-values with corresponding rows from the data set of x-values.
#   (b) On the data set from part (a), generate a multiple regression model with
#       all of the x-values as explanatory variables.
#   (c) Determine the number of significant explanatory variables at the 5% level.
#   (d) Determine and report the proportion of significant variables in the 100
#       simulations. Compare this proportion with the expected theoretical value.

library(tidyverse)

data02 <- read_csv("teamassign02data02.csv")
data02

set.seed(2)

calc_number_sig <- function(x){
  
  y <- rnorm(100, mean = 10, sd = 5)
  
  df <- as_tibble(cbind(y,data02))
  
  lm.1 <- lm(y ~ ., data = df)
  
  var_probs <- pull(as_tibble(summary(lm.1)$coefficients)[4])[-1]
  
  length(var_probs[var_probs < 0.05])
  
}

sum(sapply(1:100, calc_number_sig))
# 100 total significant variables at 5% level, across all 100 iterations together

# Expected theoretical number: 100


