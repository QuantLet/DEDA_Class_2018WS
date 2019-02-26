###################################################################
## Code to replicate the results in the Barunik and Kley (2018)
###################################################################
##
## Auxiliary Functions definition 
##
## Authors: Jozef Barunik, and Tobias Kley (2018)
###################################################################

library(alphahull)
library(astsa)
library(copula)
library(latex2exp)
library(mAr)        # for simulation of VAR 
library(pbivnorm)
library(quantreg)
library(quantspec)
library(rugarch)
library(vars)       # for estimation of VAR
library(zoo)

# Define various Data Generating Processes

indep &lt;- function(n, rho = 0) {
  Y1 &lt;- rnorm(n)
  Y2 &lt;- rho * Y1 + sqrt(1 - rho^2) * rnorm(n)
  res &lt;- matrix(c(Y1, Y2), ncol = 2)
  return(as.matrix(res))
}

indepq &lt;- function(n) {  
  y1 &lt;- rnorm(n)
  y2 &lt;- (y1^2 - 1) / 3
  return(cbind(y1, y2))  
}

indepqshift &lt;- function(n) {
  y1 &lt;- rnorm(n+1)
  y2 &lt;- (y1[2:(n+1)]^2 - 1) / 3
  return(cbind(y1[1:n], y2))
}

transl &lt;- function (rho,tau1,tau2){
  p12 &lt;- pbivnorm(qnorm(tau1, 0, 1), qnorm(tau2, 0, 1), rho)
  return( (p12-tau1*tau2)/(sqrt(tau1*(1-tau1))*sqrt(tau2*(1-tau2))) )
}

indepR &lt;- function(n, rho=0.6) {
  Y1 &lt;- rnorm(n)
  Y2 &lt;- rho * Y1 + sqrt(1 - rho^2) * rnorm(n)
  res &lt;- matrix(c(Y1, Y2), ncol = 2)
  return(as.matrix(res))
}



quar31&lt;-function (n, 
                  th11 = function(u) { 0 * ((u - 0.5))}, 
                  th12 = function(u) { 1.2 * ((u - 0.5))},
                  th21 = function(u) { 1.2 * ((u - 0.5))}, 
                  th22 = function(u) { 0 * ((u - 0.5))},
                  
                  ga11 = function(u) { 0 * ((u - 0.5))},
                  ga12 = function(u) { 0 * ((u - 0.5))},
                  ga21 = function(u) { 0 * ((u - 0.5))},
                  ga22 = function(u) { 0 * ((u - 0.5))},
                  overhead = 1000, th01 = qnorm,th02 = qnorm,rho = 0) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:2] &lt;- th01(runif(2))
  Y2[1:2] &lt;- th02(runif(2))
  for (t in 3:(n + overhead)) {
    U1 &lt;- runif(1)
    U2 &lt;- runif(1)
    Y1[t] &lt;- th11(U1) * Y1[t - 1] + th12(U1) * Y2[t - 1] +
             ga11(U1) * Y1[t - 2] + ga12(U1) * Y2[t - 2] + th01(U1)
    Y2[t] &lt;- th21(U2) * Y1[t - 1]  + th22(U2) * Y2[t - 1] +
             ga21(U2) * Y1[t - 2]  + ga22(U2) * Y2[t - 2] + th02(U2)
  }
  Y &lt;- cbind(Y1[(overhead + 1):(overhead + n)],
             Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}

quar32&lt;-function (n, 
                  th11 = function(u) { 0 * ((u - 0.5))}, 
                  th12 = function(u) { 0 * ((u - 0.5))},
                  th21 = function(u) { 0 * ((u - 0.5))}, 
                  th22 = function(u) { 0 * ((u - 0.5))},
                  
                  ga11 = function(u) { 0 * ((u - 0.5))},
                  ga12 = function(u) { 1.2 * ((u - 0.5))},
                  ga21 = function(u) { 1.2 * ((u - 0.5))},
                  ga22 = function(u) { 0 * ((u - 0.5))},
                  overhead = 1000, th01 = qnorm, th02 = qnorm, rho = 0) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:2] &lt;- th01(runif(2))
  Y2[1:2] &lt;- th02(runif(2))
  for (t in 3:(n + overhead)) {
    U1 &lt;- runif(1)
    U2 &lt;- runif(1)
    Y1[t] &lt;- th11(U1) * Y1[t - 1] + th12(U1) * Y2[t - 1] +
             ga11(U1) * Y1[t - 2] + ga12(U1) * Y2[t - 2] + th01(U1)
    Y2[t] &lt;- th21(U2) * Y1[t - 1]  + th22(U2) * Y2[t - 1] +
             ga21(U2) * Y1[t - 2]  + ga22(U2) * Y2[t - 2] + th02(U2)
  }
  Y &lt;- cbind(Y1[(overhead + 1):(overhead + n)],
             Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}

quar33&lt;-function (n, 
                  th11 = function(u) { 0 * ((u - 0.5))}, 
                  th12 = function(u) { 0 * ((u - 0.5))},
                  th21 = function(u) { 0 * ((u - 0.5))}, 
                  th22 = function(u) { 0 * ((u - 0.5))},
                  
                  ga11 = function(u) { 0 * ((u - 0.5))},
                  ga12 = function(u) { 1.2 * ((u - 0.5))},
                  ga21 = function(u) { 1.2 * ((u - 0.5))},
                  ga22 = function(u) { 0 * ((u - 0.5))},
                  overhead = 1000, th01 = qnorm, th02 = qnorm, rho = 0) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:3] &lt;- th01(runif(2))
  Y2[1:3] &lt;- th02(runif(2))
  for (t in 4:(n + overhead)) {
    U1 &lt;- runif(1)
    U2 &lt;- runif(1)
    Y1[t] &lt;- th11(U1) * Y1[t - 1] + th12(U1) * Y2[t - 1] +
             ga11(U1) * Y1[t - 3] + ga12(U1) * Y2[t - 3] + th01(U1)
    Y2[t] &lt;- th21(U2) * Y1[t - 1]  + th22(U2) * Y2[t - 1] +
             ga21(U2) * Y1[t - 3]  + ga22(U2) * Y2[t - 3] + th02(U2)
  }
  Y &lt;- cbind(Y1[(overhead + 1):(overhead + n)],
          Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}


###############################################################################
# The function below computes
#
#		(1) the traditional spectrum
#		(2) the quantile spectrum
#	
#	for a VAR(1) process with
#
# 	X_t = A X_{t-1} + eps_t
#
# 	where eps_t is white noise with Var(eps_t) = Sigma
# 
# Author: Tobias Kley
###############################################################################

getTradQuantCoherencyInVAR1 &lt;- function(A, Sigma, levels, maxLag = 127) {
  
  D &lt;- ncol(A)
  
  Gamma0 &lt;- matrix(solve(diag(rep(1, D^2)) - A %x% A) %*% as.vector(Sigma),
                   ncol = D)
  
  ## determine all (traditional) Gamma_k
  
  Gamma &lt;- array(0, dim = c(maxLag, D, D))
  
  Gamma[1,,] &lt;- A %*% Gamma0 
  for (i in 2:maxLag) {
    Gamma[i,,] &lt;- A %*% Gamma[i-1,,]
  }
  
  ## next compute copula cross-covariances
  
  K &lt;- length(levels)
  
  qGamma0 &lt;- array(0, dim = c(D, K, D, K))
  qGamma &lt;- array(0, dim = c(maxLag, D, K, D, K)) 
  
  Rho0 &lt;- array(0, dim = c(D, D))
  Rho &lt;- array(0, dim = c(maxLag, D, D))
  
  for (i1 in 1:D) {
    for (i2 in 1:D) {
      Rho0[i1,i2] &lt;- Gamma0[i1,i2] / sqrt(Gamma0[i1,i1] * Gamma0[i2,i2])  
      Rho[,i1,i2] &lt;- Gamma[,i1,i2] / sqrt(Gamma0[i1,i1] * Gamma0[i2,i2]) 
    }
  }
  
  for (i1 in 1:D) {
    for (k1 in 1:K) {
      for (i2 in 1:D) {
        for (k2 in 1:K) {
          tau1 &lt;- levels[k1]
          tau2 &lt;- levels[k2]
          
          rho &lt;- Rho0[i1,i2]
          p12 &lt;- pbivnorm(qnorm(tau1,0,1), qnorm(tau2,0,1), rho)
          qGamma0[i1,k1,i2,k2] &lt;- (p12-tau1*tau2) /
              (sqrt(tau1*(1-tau1))*sqrt(tau2*(1-tau2)))
          
          rho &lt;- Rho[,i1,i2]
          p12 &lt;- pbivnorm(qnorm(tau1,0,1), qnorm(tau2,0,1), rho)
          qGamma[,i1,k1,i2,k2] &lt;- (p12-tau1*tau2) /
              (sqrt(tau1*(1-tau1))*sqrt(tau2*(1-tau2)))       
        }
      }
    }
  }
  
  ## now compute the spectra
  # (1) traditional spectra
  
  tradSpec &lt;- array(0, dim=c(maxLag+1, D, D))
  quantSpec &lt;- array(0, dim=c(maxLag+1, D, K, D, K))
  
  tradCoh &lt;- array(0, dim=c(maxLag+1, D, D))
  quantCoh &lt;- array(0, dim=c(maxLag+1, D, K, D, K))
  
  
  for (i1 in 1:D) {
    for (i2 in 1:D) {
      AA &lt;- fft(c(Gamma0[i1,i2], Gamma[,i1,i2]))
      BB &lt;- Conj(fft(c(Gamma0[i2,i1], Gamma[,i2,i1]))) - Gamma0[i2,i1]
      tradSpec[,i1,i2] &lt;- AA + BB
      
      for (k1 in 1:K) {
        for (k2 in 1:K) {
          AA &lt;- fft(c(qGamma0[i1,k1,i2,k2], qGamma[,i1,k1,i2,k2]))
          BB &lt;- Conj(fft(c(qGamma0[i1,k1,i2,k2], qGamma[,i1,k1,i2,k2]))) -
              qGamma0[i1,k1,i2,k2]
          quantSpec[,i1,k1,i2,k2] &lt;- AA + BB
        }
      }
    }
  }
  tradSpec &lt;- tradSpec / (2*pi)
  quantSpec &lt;- quantSpec / (2*pi)
  
  ## now compute coherency
  
  tradCoh &lt;- array(0, dim=c(maxLag+1, D, D))
  quantCoh &lt;- array(0, dim=c(maxLag+1, D, K, D, K))
  
  for (i1 in 1:D) {
    for (i2 in 1:D) {
      tradCoh[,i1,i2] &lt;- tradSpec[,i1,i2] /
          sqrt(Re(tradSpec[,i1,i1]) * Re(tradSpec[,i2,i2]))
      for (k1 in 1:K) {
        for (k2 in 1:K) {
          
          quantCoh[,i1,k1,i2,k2] &lt;- quantSpec[,i1,k1,i2,k2] /
              sqrt(Re(quantSpec[,i1,k1,i1,k1]) * Re(quantSpec[,i2,k2,i2,k2]))
        }
      }
    }
  }
  return( list(tradSpec = tradSpec, quantSpec = quantSpec,
               tradCoh = tradCoh, quantCoh = quantCoh) )
  
}


# functiono to determine points on the alpha convex hull:
aConvHull &lt;- function(X, a = 0.1) {
  Xu &lt;- unique(round(X,5))
  
  alsh &lt;- ashape(x=Xu[,1], y=Xu[,2], alpha = a)
  
  Pts &lt;- alsh$edges[,3:6]
  
  Y &lt;-rbind(Pts[1,1:2], Pts[1,3:4])
  Pts &lt;- Pts[-1,]
  
  repeat {
    
    if (sum((Pts[,1] == Y[nrow(Y),1]) + (Pts[,2] == Y[nrow(Y),2])) &gt; 1) {
      j &lt;- which(Pts[,1] == Y[nrow(Y),1] &amp; Pts[,2] == Y[nrow(Y),2])
      Y &lt;- rbind(Y, Pts[j[1],3:4])
    } else if (sum((Pts[,3] == Y[nrow(Y),1]) + (Pts[,4] == Y[nrow(Y),2])) &gt; 1) {
      j &lt;- which(Pts[,3] == Y[nrow(Y),1] &amp; Pts[,4] == Y[nrow(Y),2])
      Y &lt;- rbind(Y, Pts[j[1],1:2])
    } else {
      stop("something is wrong here!")
    }
    if (length(j) &gt; 1) {break}
    
    Pts &lt;- Pts[-j[1],]
    if (!is.matrix(Pts)) {break}
  }
  
  return (rbind(Y,Y[1,])) 
}
# END: (1) determine points on the alpha convex hull:




###############################################################################
## DEFINE AUXILIARY FUNCTIONS for estimation of QVAR
###############################################################################

##simulate a QVAR(1), WITHOUT the spacial component
rqvar1 &lt;- function (n, th10, th11, th12, th20, th21, th22,
                    myCop, overhead = 1000) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:2] &lt;- th10(runif(2))
  Y2[1:2] &lt;- th20(runif(2))
  
  innov = rCopula(n + overhead, myCop)
  
  U01 &lt;- innov[,1]
  U02 &lt;- innov[,2]
  
  for (t in 3:(n + overhead)) {
    U1 &lt;- U01[t]
    U2 &lt;- U02[t]
    Y1[t] &lt;- th10(U1) + th11(U1) * Y1[t - 1] + th12(U1) * Y2[t - 1]
    Y2[t] &lt;- th20(U2) + th21(U2) * Y1[t - 1] + th22(U2) * Y2[t - 1]
  }
  Y &lt;- cbind(Y1[(overhead + 1):(overhead + n)],
             Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}

## simulate a QVAR(1), WITH spatial components in Y_1 and Y_2
rqvar2 &lt;- function (n, th10, th120, th111, th121, th20, th210, th211, th221,
                    myCop, overhead = 1000) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:2] &lt;- 0 
  Y2[1:2] &lt;- 0 
  
  innov = rCopula(n + overhead, myCop)
  
  U01 &lt;- innov[,1]
  U02 &lt;- innov[,2]
  
  for (t in 3:(n + overhead)) {
    U1 &lt;- U01[t]
    U2 &lt;- U02[t]
    
    d1 &lt;- 1 - th120(U1) * th210(U2)
    b1 &lt;- th10(U1) + th120(U1) * th20(U2)
    a11 &lt;- th111(U1) + th120(U1) * th211(U2)
    a12 &lt;- th121(U1) + th120(U1) * th221(U2)
    Y1[t] &lt;- (b1 + a11 * Y1[t - 1] +  a12 * Y2[t - 1]) / d1
    
    d2 &lt;- 1 - th210(U2) * th120(U1)
    b2 &lt;- th20(U2) + th210(U2) * th10(U1)
    a21 &lt;- th211(U2) + th210(U2) * th111(U1)
    a22 &lt;- th221(U2) + th210(U2) * th121(U1)
    Y2[t] &lt;- (b2 + a21 * Y1[t - 1] + a22 * Y2[t - 1]) / d2
  }
  Y=cbind(Y1[(overhead + 1):(overhead + n)],
          Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}

## simulate a QVAR(1), WITH a spatial component in Y_1
rqvar2a &lt;- function (n, th10, th120, th111, th121, th20, th211, th221,
                     myCop, overhead = 1000) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:2] &lt;- 0 
  Y2[1:2] &lt;- 0 
  
  innov = rCopula(n + overhead, myCop)
  
  U01 &lt;- innov[, 1]
  U02 &lt;- innov[, 2]
  
  for (t in 3:(n + overhead)) {
    U1 &lt;- U01[t]
    U2 &lt;- U02[t]
    Y2[t] &lt;- th20(U2) + th211(U2) * Y1[t - 1] + th221(U2) * Y2[t - 1]
    Y1[t] &lt;- th10(U1) + th120(U1) * Y2[t]
    + th111(U1) * Y1[t - 1] + th121(U1) * Y2[t - 1]
  }
  Y &lt;- cbind(Y1[(overhead + 1):(overhead + n)],
             Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}

## simulate a QVAR(1), WITH a spatial component in Y_2
rqvar2b &lt;- function (n, th10, th111, th121, th20, th210, th211, th221,
                     myCop, overhead = 1000) 
{
  Y1 &lt;- rep(0, n + overhead)
  Y2 &lt;- rep(0, n + overhead)
  Y1[1:2] &lt;- 0 
  Y2[1:2] &lt;- 0 
  
  innov = rCopula(n + overhead, myCop)
  
  U01 &lt;- innov[,1]
  U02 &lt;- innov[,2]
  
  for (t in 3:(n + overhead)) {
    U1 &lt;- U01[t]
    U2 &lt;- U02[t]
    Y1[t] &lt;- th10(U1) + th111(U1) * Y1[t - 1] + th121(U1) * Y2[t - 1]
    Y2[t] &lt;- th20(U2) + th210(U2) * Y1[t]
    + th211(U2) * Y1[t - 1] + th221(U2) * Y2[t - 1]
  }
  Y &lt;- cbind(Y1[(overhead + 1):(overhead + n)],
             Y2[(overhead + 1):(overhead + n)])
  return(as.matrix(Y))
}
