# library(quantreg)

loss.qr.eqn&lt;-function(y,l_t,p){
summ&lt;-0
for(k in 1:length(y)){
summ&lt;-(p-as.numeric(y[k]&lt;l_t[k]))*(y[k]-l_t[k])+summ
}
return(loss=summ)
}

GACV.qr&lt;-function(lamb,x,y,p){
n&lt;-length(y)
fit&lt;-rq.fit.lasso(x,y,tau=p,lambda=lamb)
D&lt;-0
est&lt;-x%*%fit$coefficients
L&lt;-loss.qr.eqn(y,est,p)
D&lt;-length(which(abs(y[i]-est[i])&lt;0.000001))
return(loss=L/(n-D))
}
