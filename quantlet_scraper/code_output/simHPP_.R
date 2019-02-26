simHPP = function(lambda,T,N){
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
