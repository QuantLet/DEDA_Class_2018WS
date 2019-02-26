mixexprnd = function(p,beta1,beta2,n,m){

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

  y=matrix(y,n,m)
  return(y)
  }
