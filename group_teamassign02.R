
###########################
#                         #
#   Team Assignment 2     #
#                         #
###########################

# Group 7 | Kennan Grant ,	Elizabeth Homan ,	Adrian Mead ,	Gregory Wert

library(plyr)
library(tidyverse)
set.seed(1)

#################
## Question 1: ##
#################

# For this problem you will use the data in the file "teamassign02data01.csv" to implement   
# a simple form of the bootstrap resampling method.  Repeat (a) and (b) 1000 times:

#set the repeat size
n <- 1000

#   (a) From the data set, select **with replacement** 100 random pairs (x,y).
#       You will have some repeats -- which is OK and expected.

#read in the data
p1 <- read.csv('teamassign02data01.csv')

#take a random sample of pairs
samples <- lapply(X = 1:n, function(X){
  s <- p1[sample(nrow(p1),size=100,replace=TRUE),]
  return(list(data.frame(s)))})

#   (b) Use your sample to generate a regression equation. Save the values of 
#       hat(beta_0) and hat(beta_1).

#Create models from the samples
models <- lapply(X = 1:n, function(X) lm(y~x, data = samples[[X]][[1]]))

#extract the betas from the models
beta0_l <- unlist(lapply(models, function(X) as.vector(X$coefficients)[1]))
beta1_l <- unlist(lapply(models, function(X) as.vector(X$coefficients)[2]))

#assemble the betas in a dataframe
df_betas <- data.frame("beta_0" = beta0_l, "beta_1"=beta1_l)

#   (c) Find and report a 95% confidence interval for beta_0 and beta_1 by determining
#       the 2.5th and 97.5th percentiles for each set of values.  Do the confidence 
#       intervals contain the true parameter values?

#calculate the error term on the interval for beta0
error_beta_0 <- qt(0.975,df=length(df_betas$beta_0)-1)*sd(df_betas$beta_0)/sqrt(length(df_betas$beta_0))

#construct the left and right boundaries of a 95% interval
left_beta_0 <- mean(df_betas$beta_0)-error_beta_0
# the lower bound is 21.0908
right_beta_0 <- mean(df_betas$beta_0)+error_beta_0
# the upper bound is 21.6102
#Beta0 CI : [21.0908,21.6102]

#repeat the process for beta1
error_beta_1 <- qt(0.975,df=length(df_betas$beta_1)-1)*sd(df_betas$beta_1)/sqrt(length(df_betas$beta_1))
left_beta_1 <- mean(df_betas$beta_1)-error_beta_1
#the lower bound is 4.06556
right_beta_1 <- mean(df_betas$beta_1)+error_beta_1
#the upper bound is 4.07806
#Beta1 CI : [4.06556,4.07806]

#creat a model to get the true model coefficients
true_lm <- lm(y~x, data=p1)

#extract the true coefficients
beta0_true <- summary(true_lm)[[4]][1]
beta1_true <- summary(true_lm)[[4]][1]
#beta0 = 21.2737, beta1 = 4.0729
#Yes, the intervals contain the true parameter values 

#################
## Question 2: ##
#################

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
# OUTPUT: 1
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