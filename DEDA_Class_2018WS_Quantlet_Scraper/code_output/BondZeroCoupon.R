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


simNHPP = function(lambda,parlambda,T,N){
  	
    # SIMNHPP Non-homogeneous Poisson process.
    # ---------------------------------------------------------------------
    #       Y = SIMNHPP(lambda,parlambda,T,N) generates N trajectories of the
    #       non-homogeneous Poisson process with intensity specified by LAMBDA
    #       (0 - sine function, 1 - linear function, 2 - sine square function)
    #       with paramters in PARLAMBDA. T is the time horizon. The function
    #       usues thining method.
    # ---------------------------------------------------------------------
    
    a = parlambda[1]
    b = parlambda[2]
    if (lambda==0){
        d = parlambda[3]
        JM = simHPP(a+b,T,N)
    } else if(lambda==1){
        JM = simHPP(a+b*T,T,N)
    } else if (lambda==2){
        d = parlambda[3]
        JM = simHPP(a+b*T,T,N)
	}
	rjm = nrow(JM)
    yy = array(0,c(rjm,N,2))
    yy[,,1]= matrix(T,nrow=rjm,ncol=N)
    
    i=1
    maxEN=0
    while(i&lt;=N){
        pom = JM[,i,1][JM[,i,1]
<t] !="Burr" #="" #[1:i,])="" #paretornd="" &&="" (distrib="Burr" (i="" (lambda="2){" (lambda)="1){" (lambda)!="1){" (length(n))!="1){" (length(params))!="1){" (length(t))!="1){" (missing(alpha)){="" (missing(lambda)){="" (missing(m)){="" (missing(n)){="" )="" ){="" 0="" 0,1="" 1="" 1,="" 1:m){="" 2="" 2,="" 2.")="" 3="" <="0" a="" alpha="1" alpha,="" an="" and="" array="" arrays="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" be="" be:="" burr="" burr")="" chosen="" cpp="dim(poisproc)[2]" default="" distrib="" distribs="" distribution="" distribution,="" distribution.="" distributions,="" either="" else="" en="NROW(pom)" exponential="" exponential,="" for="" from="" gamma,="" i="1" if="" if((distrib="gamma" if(aux="" if(distrib="" if(lambda="" if(length(parlambda)!="2" if(n="1){" if(t="" in="" inversion="" lambda="" lambda!="2){" lambda,="" lambda.="" lambdat="(a+b*sin(2*pi*(pom+d))^2)/(a+b)" lognormal,="" losses="matrix(0,rpp,cpp)" m="1" m-by-n="" maxen="max(maxEN,EN)" method.="" mixofexps="" mixofexps,="" must="" n="" n,="" numbers="" of="" or="" parameters="" params="" pareto="" paretornd="" parlambda="" poisproc="simNHPP(lambda,parlambda,T,N)" pom="pom[R&lt;lambdat]" positive="" r="runif(NROW(pom))" random="" respectively.="" return(y)="" return(yy)="" returns="" rpp="dim(poisproc)[1]" scalar.")="" should="" simnhppalp="function(lambda,parlambda,distrib,params,T,N){" stop("simnhppalp:="" t="" the="" u="matrix(0,n,m)" u[,i]="lambda*(runif(n,0,1)^(-1/alpha)-1)" uses="" values="" vector.")="" weibull="" weibull,="" while(i<="N){" with="" x="" y="u" yy="yy[1:(2*maxEN+2),,]" yy[(2*en+1):rjm,i,2]="matrix(EN,nrow=rjm-2*EN,ncol=1)" yy[1:(2*en+1),i,1]="c(0,rep(pom,each=2))" yy[2:(2*en),i,2]="c(floor((1:(2*EN-1))/2))" {="" ||="" }="" }else{="" }}}="">
 2){
        laux                  = cumsum(Burrrnd(params[1],params[2],params[3],aux/2-1,1))
        losses[3:aux,i]       = laux[ceiling((1:(aux-2))/2)]
        if(aux
 <rpp){ #[1:i,])="" ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="exponential" if(n="1){" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
  2){
        laux = cumsum(rexp(aux/2-1,rate=1/params[1]))
        losses[3:aux,i]=laux[ceiling((1:aux-2)/2)]
        if(aux
  <rpp){ #[1:i,])="" ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="gamma" if(n="1){" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
   2){
        laux = cumsum(rgamma(aux/2-1,shape=params[1],rate=params[2],scale=(1/params[2])))
        losses[3:aux,i] = laux[ceiling((1:aux-2)/2)]
        if(aux
   <rpp){ #[1:i,])="" ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="lognormal" if(n="1){" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
    2){
        laux = cumsum(rlnorm(aux/2-1,meanlog=params[1],sdlog=params[2]))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux
    <rpp){ #[1:i,])="" ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="mixofexps" if(n="1){" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
     2){
        laux = cumsum(mixexprnd(params[3],params[1],params[2],aux/2-1,1))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux
     <rpp){ #[1:i,])="" ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="Pareto" if(n="1){" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
      2){
        laux=cumsum(Paretornd(params[1],params[2],aux/2-1,1))
        losses[3:aux,i] = laux[ceiling((1:(aux-2))/2)]
        if(aux
      <rpp){ #[1:i,])="" ){="" aux="min(as.matrix(which(poisproc[,i,1]==T)))" i="1" if(aux="" if(distrib="Weibull" if(n="1){" losses[(aux+1):rpp,i]="matrix(laux[length(laux)],rpp-aux,1)" losses[,i]="rep(0,rpp)" while(i<="N){" }="" }else="" }else{="">
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
  if(N==1){
  	y     = array(0,dim(poisproc))
    y[,1] = poisproc[,1]
    y[,2] = losses[,1]
  }else{
  	y     = array(0,dim(poisproc))
  	y[,,1] = poisproc[,,1]
    y[,,2] = losses
  }
  return(y)
  }


########################## MAIN PROGRAM ##########################


BondZeroCoupon = function(Z,D,T,r,lambda,parlambda,distr,params,Tmax,N){

  if(lambda != 0 &amp;&amp; lambda != 1 &amp;&amp; lambda!=2){
  	stop("BondZeroCoupon: Lambda must be either 0,1 or 2.")
  }
  if(length(Z) !=1){
  	stop("BondZeroCoupon: payment at maturity Z needs to be a scalar")
  }
  if(length(r) !=1){
  	stop("BondZeroCoupon: discount rate needs to be a scalar")
  }
  if(dim(as.matrix(D))[1]==1){
  	stop("BondZeroCoupon: threshold level D needs to be a vector ")
  }
  if(dim(as.matrix(T))[1]==1){
  	stop("BondZeroCoupon: time to expiry T needs to be a vector ")
  } 

  
  x   = simNHPPALP(lambda,parlambda,distr,params,Tmax,N)
  Tl  = length(T)
  Dl  = length(D)
  y   = matrix(0,Tl*Dl,3)
  i   = 1 #loop (times to maturity)
  j   = 1 #loop (treshold levels)
  k   = 1 #loop (trajectories)
  wyn = 0
  while(i&lt;=Tl){
    while(j&lt;=Dl){
      while(k&lt;=N){
        traj = cbind(x[,k,1],x[,k,2])
        wyn  = wyn+as.numeric(traj[length(traj[which(traj[,1]&lt;=T[i]),1]),2]&lt;=D[j])
        k = k+1
      }
      y[(i-1)*Dl+j,1]=T[i]
      y[(i-1)*Dl+j,2]=D[j]
      y[(i-1)*Dl+j,3]=Z*exp(-r*T[i])*wyn/N#
      wyn = 0
      k   = 1
      j   = j+1
    }
    j = 1
    i = i+1
  }
  y = y
  return(y)
}
      </rpp){>
     </rpp){>
    </rpp){>
   </rpp){>
  </rpp){>
 </rpp){>
</t]>