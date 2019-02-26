### This generates the Simulation Data as a dataframe
# by Daniel Jacob (daniel.jacob@hu-berlin.de) 

# Arguments to specify are: 

# N = Number of observations (real number)
# k = Number of covariates (real number)
# Y = "binary" for binary output levels, or "1" for continuous level
# random_d = treatment assignment: (Either T for random assignment or F for confounding on X)
# theta = treatment effect: (Either real number for only one theta, or "binary" {0.1,0.3} or "con" for continuous values (0.1,0.3))
# var = Size of the variance (Noise-level)

#Required Packages
if(!require("clusterGeneration")) install.packages("clusterGeneration"); library("clusterGeneration")
if(!require("mvtnorm")) install.packages("mvtnorm"); library("mvtnorm")



datagen &lt;- function(N,y,k,random_d,theta,var) {
  
  N = N
  k = k
  b = 1 / (1:k)
  # = Generate covariance matrix of z = #
  sigma &lt;- genPositiveDefMat(k, "unifcorrmat")$Sigma
  sigma &lt;- cov2cor(sigma)
  
  
  z &lt;- rmvnorm(N, sigma = sigma) # = Generate z = #
  
  
  ### Options for D (m_(X))
  if (random_d == T) {
    d &lt;- rep(c(0, 1), length.out = N)
  } else {
    d_prop &lt;- pnorm(z %*% b) # D is dependent on Z
    d &lt;- as.numeric(rbinom(N, prob = d_prop, size = 1))
  }
  
  
  ### Options for theta
  if (theta == "con") {
    theta_s &lt;- as.vector(sin(z %*% b) ^ 2)
    theta &lt;-
      (theta_s - min(theta_s)) * (0.3 - 0.1) / (max(theta_s) - min(theta_s)) +
      0.1
  } else if (theta == "binary") {
    theta_low &lt;-
      rbinom(N, pnorm((z[, 6] * (z[, 1] %*% t(
        z[, 5]
      )) * z[, 2]) ^ 2), size = 1)
    theta &lt;-
      ifelse(theta_low == 1, 0.3, 0.1)
  } else if (theta == "big") {
    theta_big &lt;-
      rbinom(N, pnorm((z[, 6] * (z[, 1] %*% t(
        z[, 5]
      )) * z[, 2]) ^ 2), size = 1)
    theta &lt;- ifelse(theta_big == 1, 1, 0.4)
  }  else {
    theta == theta
  }
  
  
  g &lt;- as.vector(cos(z %*% b) ^ 2)
  
  if(y=="binary") {
    y1 &lt;- theta * d + g + pnorm(rnorm(N,0,var))
    y1.1 &lt;- rbinom(N,prob=pnorm(scale(y1)),size=1)
    #y1.1 &lt;- (y1 - min(y1)) * (1) / (max(y1) - min(y1)) + 0
    #y &lt;-  rbinom(N,prob=y1.1,size=1)
    y &lt;- y1.1
  } else {y &lt;- y1}
  
  data &lt;- as.data.frame(y)
  data &lt;- cbind(data, theta, d, z)
  colnames(data) &lt;- c("y", "theta", "d", c(paste0("V", 1:k)))
  
  return(data)
}


### Example
dataset &lt;- datagen(y="binary",N = 2000, k = 20, random_d = F, theta = "binary", var = 1)
summary(dataset)
str(dataset)
