# ---------------------------------------------------------------------
# Book         STF
# ---------------------------------------------------------------------
# See also:    simHPP
# ---------------------------------------------------------------------
# Quantlet:    simNHPP
# ---------------------------------------------------------------------
# Description: simNHPP generates non-homogeneous Poisson process.
# ---------------------------------------------------------------------
# Usage:       y = simNHPP(lambda,parlambda,T,N)
# ---------------------------------------------------------------------
# Input:     
# Parameter:   lambda
# Definition:  scalar, intensity function, sine function (lambda=0)
#              linear function (lambda=1) or sine square function (lambda=2)
# Parameter:   parlambda
# Definition:  n x 1 vector, parameters of the intensity function lambda
#              (n = 2 for lambda = 1, n = 3 otherwise)
# Parameter:   T
# Definition:  scalar, time horizon
# Parameter:   N
# Definition:  scalar, number of trajectories
# ---------------------------------------------------------------------
# Output:      
# Parameter:   y
# Definition:  2*max+2 x N x 2 array, generated process - max is the
#              maximum number of jumps for all generated trajectories 
# ---------------------------------------------------------------------
# Example:     
#              y1 = simNHPP(0,c(1,1,0),5,1)
#              y2 = simNHPP(1,c(1,1),5,1)
#              plot(y1[,1],y1[,2],type="l",col="red3",ylim=c(min(y1[,2],y2[,2]),max(y1[,2],y2[,2])),ylab="",xlab="")
#              lines(y2[,1],y2[,2],type="l",col="blue3")
# ---------------------------------------------------------------------
# Result:      Show two trajectories of non-homogeneous Poisson process
# ---------------------------------------------------------------------
# Keywords:    Poisson process, risk process
# ---------------------------------------------------------------------
# Reference:   K. Burnecki, R.Weron (2004) "Modeling of the risk process",
#	           in "Statistical Tools for Finance and Insurance", 
#              eds. P. Cizek, W. Härdle, R. Weron, Springer. 
# ---------------------------------------------------------------------
# Author:      Awdesch Melzer 20130628
# ---------------------------------------------------------------------


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
        pom = JM[,i,1][JM[,i,1]&lt;T]
        pom = pom[2*(1:(length(pom)/2))]
        R = runif(NROW(pom))
        if (lambda==0){
            lambdat = (a+b*sin(2*pi*(pom+d)))/(a+b)
        } else {
            if (lambda==1){
                lambdat = (a+b*pom)/(a+b*T)
            } else {
                if (lambda==2){
                    lambdat = (a+b*sin(2*pi*(pom+d))^2)/(a+b)
                }}}
        pom = pom[R&lt;lambdat]
        EN = NROW(pom)
        maxEN = max(maxEN,EN)
        yy[1:(2*EN+1),i,1] = c(0,rep(pom,each=2))
        yy[2:(2*EN),i,2] = c(floor((1:(2*EN-1))/2))
        yy[(2*EN+1):rjm,i,2] = matrix(EN,nrow=rjm-2*EN,ncol=1)
        i=i+1
    }
    yy = yy[1:(2*maxEN+2),,]
    return(yy)
}
