rm(list=ls(all=T))
graphics.off()

setwd("")

##20 step GIRF
load("mydate.RData")
#load("result3_IRG.RData")#20 steps
load("GIR_30s.RData")
coname= c("MSFT","T","IBM", "JNJ","PFE","MRK","JPM","WFC","C")
lobname = c("p", "as1","bs1","as2","bs2","as3","bs3")
na1 =rep(lobname, 9)
na2 = rep(coname, each=7)
myname=myname1 = paste(na2, na1,sep="_")

###plot of market impacts 

#myplot1: price &amp; sizes
myplot1 &lt;- function(da, ny){
  day1=res1[[da]]
  m0 = 7*(ny-1) +1
  par(mfrow=c(3,2))
  for(i in 1:6){
    m1= m0+i
    gi1 = day1[,-(1:3),m1]
    rownames(gi1)=myname
    plot(gi1[m0,], type="h", col="blue",lwd=2, axes=F, xlab="Horizon",ylab = myname[m0], main=myname[m1], bg="transparent", ylim=c(-0.3,0.3))
    points(gi1[m0,])
    axis(1, 1:31, 0:30)
    axis(2, pos=1)
    abline(h=0, lty=2)
  }
}
##select date
for(da in 1:length(res1)){
  myplot1(da,8)
  title(main=mydate[da],outer=T)
}

#section 4.1.1
myplot11 &lt;- function(da, ny){
  day1=res1[[da]]
  m0 = 7*(ny-1) +1
  par(mfrow=c(2,3))
  #for(i in 1:6){
  for(i in c(2,4,6,1,3,5)){
    m1= m0+i
    gi1 = day1[,-(1:3),m1]
    rownames(gi1)=myname
    plot(gi1[m0,], type="h", col="blue",lwd=2, axes=F, xlab="Horizon",
         ylab = myname[m0], main=myname[m1], bg="transparent", ylim=c(-0.2,0.4))
    points(gi1[m0,])
    axis(1, 1:31, 0:30)
    axis(2, pos=1)
    abline(h=0, lty=2)
  }
}

graphics.off()
for(da in 1:length(res1)){
  myplot11(da, 9)
  mtext(mydate[da], side = 3, line = -1.25, outer = TRUE, font=2)
}

graphics.off()

#significant market impact
#myplot11(42,7)
myplot11(14,7)
mtext(mydate[14], side = 3, line = -1.25, outer = TRUE, font=2)
myplot11(1,7)
mtext(mydate[1], side = 3, line = -1.25, outer = TRUE, font=2)
myplot11(40,8)
myplot11(38,8)
mtext(mydate[38], side = 3, line = -1.25, outer = TRUE, font=2)
myplot11(34,8)
