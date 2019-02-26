# ---------------------------------------------------------------------
# Book:         STF2
# ---------------------------------------------------------------------
# Quantlet:     STF2dollargamma2D
# ---------------------------------------------------------------------
# Description:  STF2dollargamma calculates the variance vega of equally
#               weighted options' portfolio, weighted by 1/K and by 1/K^2
# ---------------------------------------------------------------------
# Usage:        STF2dollargamma2D
# ---------------------------------------------------------------------
# Inputs:       s1 - the lowest strike of options'portfolio
#               s2 - the highest strike of options'portfolio
#               n - number of strike
#               tau - maturity of a swap;
# ---------------------------------------------------------------------
# Output:       2D plot of individual dollar gammas and portfolio dollar
#               gamma for each weighting scheme
# ---------------------------------------------------------------------
# Example:      For STF2dollargamma2D(s1=10, s2=200, n=10, tau=1) a plot
#               plot of individual dollar gammas and portfolio dollar
#               gamma for each weighting scheme 
# ---------------------------------------------------------------------
# Author:       Elena Silyakova, Gregoire Njacheun, Awdesch Melzer
#               20120510
# ---------------------------------------------------------------------

rm(list=ls(all=TRUE))
graphics.off()

STF2dollargamma2D = function(s1="lower bond",s2="upper bond",n="number of strike",tau="matrity of swap")
{
 r = 0 # zero interest rate
 v = 0.25 # implied volatility
 k = seq(s1,s2, by=-(s1-s2-1)/n)
 range = s2-s1
 dollargamma = NULL
 dollargammak = NULL  
 dollargammak2 = NULL
 tv = NULL
 tv1 = NULL
 tv2 = NULL

 for(j in 1:n)  #produces gamma "curves" for each strike range
 {
  s_min = max(s1,(k[j]-range))
  s_max = min(s2,(k[j]+range))
  s = seq(s_min,s_max,by=1)

  for(i in 1:length(s)) # produces individual gamma for a given strike but different underlying price 
  {
   d1 = (log(s[i]/k[j])+(r+v^2/2)*tau)/(v*tau^0.5)
   tv[i] = (dnorm(d1)*s[i])/(2*v*tau^0.5)
   tv1[i] = (dnorm(d1)*s[i])/(2*k[j]*v*tau^0.5)
   tv2[i] = (dnorm(d1)*s[i])/(2*k[j]^2*v*tau^0.5) 
  }
   dollargamma = rbind(dollargamma,tv)
   dollargammak = rbind(dollargammak,tv1)  
   dollargammak2 = rbind(dollargammak2,tv2)
 }
 
 # calculating dollar gamma of the sum
 ###########################################
 
 cumdollargamma = dollargamma[1,]
 
 for(b in 2:(n-1)) cumdollargamma = cumdollargamma+dollargamma[b,]
 
 cumdollargamma = 2*max(dollargamma)*cumdollargamma/max(cumdollargamma)
 cumdollargammak = dollargammak[1,]
 
 for(b in 2:(n-1)) cumdollargammak = cumdollargammak+dollargammak[b,]
 
 cumdollargammak = 2*max(dollargamma)*cumdollargammak/max(cumdollargammak)
 cumdollargammak2 = dollargammak2[1,]

 for(b in 2:(n-1)) cumdollargammak2 = cumdollargammak2+dollargammak2[b,]

 cumdollargammak2 = 2*max(dollargamma)*cumdollargammak2/max(cumdollargammak2)

##############################
# plotting

 par(mfrow=c(3,1))


 
 x = seq(s1,s2,by=1)
 plot(x,dollargamma[1,],xlim=c(s1,s2), ylim=c(0,max(cumdollargamma)),col="blue",type="l",xlab="Underlying price", ylab="Dollar gamma")
 
 for (z in 2:n)
 {lines(x,dollargamma[z,],xlim=c(s1,s2), ylim=c(0,max(cumdollargamma)),type="l",xlab="Underlying price", ylab="Dollar gamma")
 } 
 lines(x,cumdollargamma,xlim=c(s1,s2), ylim=c(0,max(cumdollargamma)),type="l",xlab="Underlying price", ylab="Dollar gamma")




 
 x = seq(s1,s2,by=1)
 plot(x,dollargamma[1,],xlim=c(s1,s2), ylim=c(0,max(cumdollargammak)),col="blue",type="l",xlab="Underlying price", ylab="Dollar gamma")
 
 for (z in 2:n)
 {lines(x,dollargamma[z,],xlim=c(s1,s2), ylim=c(0,max(cumdollargammak)),type="l",xlab="Underlying price", ylab="Dollar gamma")
 } 
 lines(x,cumdollargammak,xlim=c(s1,s2), ylim=c(0,max(cumdollargammak)),type="l",xlab="Underlying price", ylab="Dollar gamma")





 
 x = seq(s1,s2,by=1)
 plot(x,dollargamma[1,],xlim=c(s1,s2), ylim=c(0,max(cumdollargammak2)),col="blue",type="l",xlab="Underlying price", ylab="Dollar gamma")
 
 for (z in 2:n)
 {lines(x,dollargamma[z,],xlim=c(s1,s2), ylim=c(0,max(cumdollargammak2)),type="l",xlab="Underlying price", ylab="Dollar gamma")
 } 
 lines(x,cumdollargammak2,xlim=c(s1,s2), ylim=c(0,max(cumdollargammak2)),type="l",xlab="Underlying price", ylab="Dollar gamma")





}

STF2dollargamma2D(10,200,10,1)
