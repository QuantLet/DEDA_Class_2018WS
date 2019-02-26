library(quantreg)

#Plot the MSE, Median error, 90% ATAE
u=seq(-2,2,0.001)
windows()
par(oma=c(0,0,0,0), mar=c(2,2,2,2))
plot(u,u^2,type="l",xlab="",ylab="",lwd=2,ylim=c(0,2))
lines(u,1/2*abs(u),col="red",lwd=2)
lines(u,0.1*ifelse(u&lt;=0,-u,0)+0.9*ifelse(u&gt;0,u,0),col="blue",lwd=2)
