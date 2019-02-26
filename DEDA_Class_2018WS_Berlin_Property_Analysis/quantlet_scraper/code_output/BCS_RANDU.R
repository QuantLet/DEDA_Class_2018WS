# -------------------------------------------------------------------------
# Book:         Basic Elements of Computational Statistics
# -------------------------------------------------------------------------
# See_also: BCS_LFG.r, BCS_boxmuller.r
# -------------------------------------------------------------------------
# Macro :
# -------------------------------------------------------------------------
# Input: 
# none
# -------------------------------------------------------------------------
# Output: 
# A vector of random numbers with less than ideal random characteristics
# -------------------------------------------------------------------------
# Description: Implementation of the RANDU random number generator developed
# by IBM in the 1960s. It is a Linear Congruential Generator procedure.
# This generator has some cearly non-random characteristics, due to badly 
# chosen starting values. 
# 
# -------------------------------------------------------------------------
# Key_words: random number generation
#
# -------------------------------------------------------------------------
#   Author: Martin Schelisch
# -------------------------------------------------------------------------

#set a seed here or inside the RANDU function
#seed = 1

#Implementation of the RANDU generator (with seed=1)
RANDU = function(n, seed=1) {
  # predefine constants
  U = NULL; a = 2^16 + 3; m = 2^31
  for (i in 1:n) {
    seed = (a * seed) %% m
    U[i] = seed / m # normalize the values to [0,1]
  }
  U
}

#Run the generator four times for 4 (insufficiently) random numbers
RANDU(4)

# install package for 3d interactive plots
install.packages("plot3D")
library("plot3D")

# simulating 9000 observations and stacking them into 3 column matrix
x = matrix(RANDU(9000), ncol = 3, byrow = TRUE)

# plotting 3d plot of the cloud
scatter3D(x[,1], x[,2], x[,3])
