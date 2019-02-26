# ---------------------------------------------------------------------
# Book:        STF
# ---------------------------------------------------------------------
# See also:    simHPP, simNHPPALP
# ---------------------------------------------------------------------
# Quantlet:    simHPPALP
# ---------------------------------------------------------------------
# Description: simHPPALP generates aggregate loss process driven by
#              the homogeneous Poisson process.
# ---------------------------------------------------------------------
# Usage:       y = simHPPALP(lambda,distrib,params,T,N)
# ---------------------------------------------------------------------
# Input:       
# Parameter:   lambda
# Definition:  scalar, intensity of the Poisson process
# Parameter:   distrib
# Definition:  string, claim size distribution
#              "Burr" - Burr distribution
#              "exponential" - exponential distribution
#              "gamma" - gamma distribution
#              "lognormal" - lognormal distribution
#              "mixofexps" - mix of exponential distributions
#              "Pareto" - Pareto distribution
#              "Weibull" - Weibull distribution
# Parameter:   params
# Definition:  n x 1 vector, parameters of the claim size distribution
#              n = 1 (exponential)
#              n = 2 (gamma, lognormal, Pareto, Weibull)
#              n = 3 (Burr, mixofexps)
# Parameter:   T
# Definition:  scalar, time horizon
# Parameter:   N
# Definition:  scalar, number of trajectories
# ---------------------------------------------------------------------
# Output:      
# Parameter:   y
# Definition:  2*max+2 x N x 2 array, generated proecss - max is the
#              maximum number of jumps for all generated trajectories 
# ---------------------------------------------------------------------
# Example:
#              set.seed(100)
#              y1 = simHPPALP(3,"Burr",c(3,2,1),5,1)
#              y2 = simHPPALP(3,"Pareto",c(1.5,2.5),5,1)
#              plot(y1[,,1],y1[,,2],type="l",col="red3",ylim=c(min(y1[,,2],y2[,,2]),max(y1[,,2],y2[,,2])),ylab="",xlab="")
#              lines(y2[,,1],y2[,,2],type="l",col="blue3")
# ---------------------------------------------------------------------
# Result:      Show two trajectories of aggregate loss process driven
#              by the homogeneous Poisson process. 
# ---------------------------------------------------------------------
# Keywords:    Poisson process, risk process
# ---------------------------------------------------------------------
# Reference:   K. Burnecki, R.Weron (2004) "Modeling of the risk process",
#	           in "Statistical Tools for Finance and Insurance", 
#              eds. P. Cizek, W. Härdle, R. Weron, Springer. 
# ---------------------------------------------------------------------
# Author:      Awdesch Melzer 20130629
# ---------------------------------------------------------------------



########################## SUBROUTINES ##########################

Burrrnd = function(alpha,lambda,tau,n,m){
#BURRRND Random arrays from Burr distribution.
#   R = BURRRND(ALPHA,LAMBDA,TAU,N,M) returns an M-by-N array of random numbers 
#   chosen from the Burr distribution with parameters ALPHA, LAMBDA, TAU.
#
#   The default values for the parameters ALPHA, LAMBDA, TAU, M, N are
#   1, 1, 2, 1, 1, respectively.
#
#   BURRRND uses the inversion method.


  if (missing(m)){
    m      = 1
  }
  if (missing(n)){
    n      = 1
  }
  if (missing(tau)){
    tau    = 2
  }
  if (missing(lambda)){
    lambda = 1
  }
  if (missing(alpha)){
    alpha  = 1
  }
   u = matrix(0,n,m)
   for (i in 1:m){
   	   u[,i] = (lambda*(runif(n,0,1)^(-1/alpha)-1))^(1/tau)
   }
   y = u
return(y)
}

mixexprnd = function(p,beta1,beta2,n,m){
#MIXEXPRND Random arrays from the mixed exponential distribution.
#   Y = MIXEXPRND(P,BETA1,BETA2,N,M) returns an M-by-N array of random numbers 
#   chosen from the mixed exponential distribution with parameters P, BETA1, BETA2.
#
#   The default values for A, BETA1, BETA2, N, M are 0.5, 1, 2, 1, 1,
#   respectively.
#
#   MIXEXPRND uses the exponential number generator.

  if (missing(p)){
      p     = 0.5
  }
  if (missing(beta1)){
      beta1 = 1
  }
  if (missing(beta2)){
      beta2 = 2
  }
  y   = rexp(n*m,rate=(1/beta2))
  aux = which(runif(n*m,0,1)&lt;=p)  

      if(!missing(aux)){
          y[aux]=rexp(length(aux),1/beta1)
      }

  y=matrix(y,n,m);
  return(y)
  }

simHPP = function(lambda,T,N){
# SIMHPP Homogeneous Poisson process.
#       Y = SIMHPP(lambda,T,N) generates N trajectories of the
#       homogeneous Poisson process with intensity LAMBDA. T is the time
#       horizon. 

if(lambda &lt;= 0 || length(lambda)!=1){
 	stop("simHPP: Lambda must be a positive scalar.")
 }
 if(T &lt;= 0 || length(T)!=1){
 	stop("simHPP: T must be a positive scalar.")
 }
 if(N &lt;= 0 || length(N)!=1){
 	stop("simHPP: N must be a positive scalar.")
 }
  EN      = rpois(N,lambda*T)
  ym      = matrix(T,2*max(EN)+2,N)
  tmp     = array(0,c(2*max(EN)+2,N,2))
  y       = tmp
  y[,,1] = ym
  y[,,2] = matrix(1,2*max(EN)+2,1)%*%t(EN)

  i=1
  while(i&lt;=N){
    if(EN[i]&gt;0){
    	ttmp = c(sort(T*runif(EN[i])))
    	y[1:(2*EN[i]+1),i,1] = c(0,ttmp[ceiling((1:(2*EN[i]))/2)])
    }else{
        y[1,i,1]             = 0
    }
    y[1:(2*EN[i]+2),i,2] = c(0,floor((1:(2*EN[i]))/2),EN[i])
    i=i+1
  }
return(y)
}

Paretornd = function(alpha,lambda,n,m){
#PARETORND Random arrays from Pareto distribution.
#   Y = PARETORND(ALPHA,LAMBDA,N,M) returns an M-by-N array of random numbers 
#   chosen from the Pareto distribution with parameters ALPHA, LAMBDA.
#
#   The default values for ALPHA, LAMBDA, N, M 1, 1, 1, 1, respectively.
#
#   PARETORND uses the inversion method.

  if (missing(m)){
    m      = 1
  }
  if (missing(n)){
    n      = 1
  }
  if (missing(lambda)){
    lambda = 1
  }
  if (missing(alpha)){
    alpha  = 1
  }
  u = matrix(0,n,m)
  for (i in 1:m){
  	u[,i] = lambda*(runif(n,0,1)^(-1/alpha)-1)
  }
  y = u
  return(y)
}

########################## MAIN PROGRAM ##########################


simHPPALP = function(lambda,distrib,params,T,N){
  
if(lambda &lt;= 0 || (length(lambda)!=1)){
	stop("simHPPALP: Lambda must be a positive scalar.")
}
if(T &lt;= 0 || (length(T)!=1)){
	stop("simHPPALP: T must be a positive scalar.")
}
if(N &lt;= 0 || (length(N)!=1)){
	stop("simHPPALP: N must be a positive scalar.")
}
if((distrib=="Burr" || distrib=="mixofexps") &amp;&amp; (length(params)!=3)){
	stop("simHPPALP: for Burr and mixofexps distributions, params must be a 3 x 1 vector.")
}
if((distrib=="gamma" || distrib=="lognormal"|| distrib=="Pareto" || distrib=="Weibull") &amp;&amp; (length(params)!=2)){
	stop("simHPPALP: for gamma, lognormal, Pareto and Weibull distributions, params must be a 2 x 1 vector.")
}
if(distrib=="exponential" &amp;&amp; (length(params)!=1)){
	stop("simHPPALP: for exponential distribution, params must be a scalar.")
}
if(distrib != "exponential" &amp;&amp; distrib != "gamma" &amp;&amp; distrib != "mixofexps" &amp;&amp; distrib != "Weibull" &amp;&amp; distrib != "lognormal" &amp;&amp; distrib !="Pareto" &amp;&amp; distrib != "Burr"){
	stop("simHPPALP: distribs should be: exponential, gamma, mixofexps, Weibull, lognormal, Pareto or Burr")
}

  poisproc = simHPP(lambda,T,N)
  rpp      = dim(poisproc)[1]
  cpp      = dim(poisproc)[2]
  losses   = matrix(0,rpp,cpp)
  
  if (distrib=="Burr"){
    i = 1
    while(i&lt;=N){
      aux = min(as.matrix(which(poisproc[,i,1]==T))) #[1:i,])
      if(aux&gt;2){
        laux                   = cumsum(Burrrnd(params[1],params[2],params[3],aux/2-1,1))
        losses[3:aux,i]        = laux[ceiling((1:(aux-2))/2)]
        if(aux
<rpp){ ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="exponential" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
 2){
        laux = cumsum(rexp(aux/2-1,rate=1/params[1]))
        losses[3:aux,i]=laux[ceiling((1:aux-2)/2)]
        if(aux
 <rpp){ ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="gamma" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
  2){
        laux = cumsum(rgamma(aux/2-1,shape=params[1],rate=params[2],scale=(1/params[2])))
        losses[3:aux,i] = laux[ceiling((1:aux-2)/2)]
        if(aux
  <rpp){ ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="lognormal" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
   2){
        laux = cumsum(rlnorm(aux/2-1,meanlog=params[1],sdlog=params[2]))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux
   <rpp){ ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="mixofexps" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
    2){
        laux = cumsum(mixexprnd(params[3],params[1],params[2],aux/2-1,1))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux
    <rpp){ ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="Pareto" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
     2){
        laux=cumsum(Paretornd(params[1],params[2],aux/2-1,1))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux
     <rpp){ ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="Weibull" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
      2){
        laux=cumsum(rweibull(aux/2-1,scale=params[1]^(-1/params[2]),shape=params[2]))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux&lt;rpp){
          losses[(aux+1):rpp,i] = matrix(laux[length(laux)],rpp-aux,1)
        }
      }else{
        losses[,i] = rep(0,rpp)
      }
      i=i+1
    }
  }
    y     = array(0,dim(poisproc))
  	y[,,1] = poisproc[,,1]
    y[,,2] = losses
  
  return(y)
  }
     </rpp){>
    </rpp){>
   </rpp){>
  </rpp){>
 </rpp){>
</rpp){>