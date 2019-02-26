rm(list=ls(all=T))
graphics.off()

setwd("")
##30 step GIRF
load("mydate.RData")
load("GIR_30s.RData")
num1=NULL;
for( ddd in 1: length(res1)){
  day1 = res1[[ddd]]
  for( i in 1:63){
    #i=1
    dim1 = day1[i,-(1:3),]^2
    num1[[i]] &lt;- colSums(dim1)#total impact of j shock
  }
}


coname= c("MSFT","T","IBM", "JNJ","PFE","MRK","JPM","WFC","C")
lobname = c("p", "as1","bs1","as2","bs2","as3","bs3")
na1 =rep(lobname, 9)
na2 = rep(coname, each=7)
myname = paste(na2, na1,sep="_")
rownames(day1)=myname


myplot12 &lt;- function(da, n1 ,n2, n3){
  day1=res1[[da]]
  m0 = 7*(c(n1,n2,n1,n3,n2,n3)-1)+1
  n0 = 7*(c(n2,n1,n3,n1,n3,n2)-1)+1
  par(mfrow=c(3,2))
  for(i in 1:6){
    m1 = m0[i]
    m2 = n0[i]
    gi1 = day1[,-(1:3),m1]
    rownames(gi1)=myname
    plot(gi1[m2,(1:31)], type="h", col="blue",lwd=2, axes=F, 
         xlab="Horizon",ylab = myname[m2], main=myname[m1], ylim=c(-0.15,0.2))
    points(gi1[m2,(1:31)])
    axis(1, 1:31, 0:30)
    axis(2, pos=1)
    abline(h=0, lty=2)
  }}



#par(mfrow=c(2,3))

graphics.off()
for(da in 1:length(res1)){
  myplot12(da,4,5,6)
  mtext(mydate[da], side = 3, line = -1.25, outer = TRUE, font=2)
}

myplot12(1,1,2,3)
myplot12(1,4,5,6)
myplot12(1,7,8,9)
mtext(mydate[1], side = 3, line = -1.25, outer = TRUE, font=2)

myplot12(19,1,2,3)
myplot12(39,1,2,3)
myplot12(38,1,2,3)
myplot12(1,1,2,3)

myplot12(16,4,5,6)
myplot12(16,4,5,6)
myplot12(16,4,5,6)
