# -------------------------------------------------------------------------
# Book         Basic Elements of Computational Statistics
# -------------------------------------------------------------------------
# Quantlet     BCS_MLRleaps.r
# -------------------------------------------------------------------------
# See_also    BCS_MLRdiagnostic
#
# -------------------------------------------------------------------------
# Macro 
# -------------------------------------------------------------------------
# Input 
# none
# -------------------------------------------------------------------------
# Output 
# Plot of number of explanatory variables against R squared.
# -------------------------------------------------------------------------
# Description: Perform stepwise regression on all subsets to compare the values
# of R squared for all combinations of explanatory variables. All combinations
# with R squared &gt;0.7 are shown in a plot of R squared against subsample size.
# 
# -------------------------------------------------------------------------
# Key_words: linear regression, variable selection
# 
# -------------------------------------------------------------------------
#   Author       
# Anastasija Tetereva
# -------------------------------------------------------------------------


#install.packages("MASS") # install package to get dataset
data( UScereal , package = "MASS") # load the dataset

fit = lm(calories ~ protein + fat + carbo + sugars, data = UScereal)
# fit the regression model

install.packages("leaps") # install package for regsubsets function
library("leaps") # load the library for regsubsets function
subset = regsubsets(calories ~ protein + fat + carbo + sugars, data = UScereal, nbest = 3)
# fit lm to all possible regresiion subsets

#pdf("../BCS_MLRleaps.pdf", width = 8, height = 8)
plot(subset)
# dev.off()
