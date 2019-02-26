# clear all variables
rm(list = ls(all = TRUE))
graphics.off()
# set the working directory
#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

# install and load packages
libraries = c( "stats", "graphics","psych", "boot")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#read return datafiles
D                      = read.csv("49IPM1970.CSV", header = TRUE, sep = ",", dec = "." )
rownames(D)            = D[ ,1]
DD                     = D[ ,-1]/100
industrynames          = read.csv("industrynames.csv", header = TRUE, sep = ",")
industrynames          = industrynames[,-1]
ff                     = read.csv("FF3factors.CSV", sep = "," )
ff                     = ff[(nrow(ff)-252):nrow(ff), ]
rownames(ff)           = ff[,1]
ff[,1]                 = ff[,2] + ff[,5]
ff                     = ff/100
#read central
#read centrality datafiles
censcore_median_in     = read.csv("eigcentrmw/cen_in_median_mw.CSV", header = TRUE, sep = ",", dec = "." )
censcore_median_out    = read.csv("eigcentrmw/cen_out_median_mw.CSV", header = TRUE, sep = ",", dec = "." )
censcore_lowertail_in  = read.csv("eigcentrmw/cen_in_lowertail_mw.CSV", header = TRUE, sep = ",", dec = "." )
censcore_lowertail_out = read.csv("eigcentrmw/cen_out_lowertail_mw.CSV", header = TRUE, sep = ",", dec = "." )
censcore_uppertail_in  = read.csv("eigcentrmw/cen_in_uppertail_mw.CSV", header = TRUE, sep = ",", dec = "." )
censcore_uppertail_out = read.csv("eigcentrmw/cen_out_uppertail_mw.CSV", header = TRUE, sep = ",", dec = "." )


#save different returns into matrix
returns_median         = matrix(0, 253, 4)
returns_lowertail      = matrix(0, 253, 4)
returns_uppertail      = matrix(0, 253, 4)

# moving window estimation
for (i in 1:253){
  centrnodeshigh_median_in        = colnames(censcore_median_in)[order(censcore_median_in[i,], decreasing = TRUE)][1:10]
  centrnodeshigh_median_out       = colnames(censcore_median_out)[order(censcore_median_out[i,], decreasing = TRUE)][1:10]
  centrnodeslow_median_in         = colnames(censcore_median_in)[order(censcore_median_in[i,], decreasing = FALSE)][1:10]
  centrnodeslow_median_out        = colnames(censcore_median_out)[order(censcore_median_out[i,], decreasing = FALSE)][1:10]

  returns_median[i,1]             = sum(DD[(312+i), centrnodeshigh_median_in])/10
  returns_median[i,2]             = sum(DD[(312+i), centrnodeshigh_median_out])/10
  returns_median[i,3]             = sum(DD[(312+i), centrnodeslow_median_in])/10
  returns_median[i,4]             = sum(DD[(312+i), centrnodeslow_median_out])/10


  centrnodeshigh_lowertail_in     = colnames(censcore_lowertail_in)[order(censcore_lowertail_in[i,], decreasing = TRUE)][1:10]
  centrnodeshigh_lowertail_out    = colnames(censcore_lowertail_out)[order(censcore_lowertail_out[i,], decreasing = TRUE)][1:10]
  centrnodeslow_lowertail_in      = colnames(censcore_lowertail_in)[order(censcore_lowertail_in[i,], decreasing = FALSE)][1:10]
  centrnodeslow_lowertail_out     = colnames(censcore_lowertail_out)[order(censcore_lowertail_out[i,], decreasing = FALSE)][1:10]

  
  returns_lowertail[i,1]          = sum(DD[(312+i), centrnodeshigh_lowertail_in])/10
  returns_lowertail[i,2]          = sum(DD[(312+i), centrnodeshigh_lowertail_out])/10
  returns_lowertail[i,3]          = sum(DD[(312+i), centrnodeslow_lowertail_in])/10
  returns_lowertail[i,4]          = sum(DD[(312+i), centrnodeslow_lowertail_out])/10

  
  centrnodeshigh_uppertail_in     = colnames(censcore_uppertail_in)[order(censcore_uppertail_in[i,], decreasing = TRUE)][1:10]
  centrnodeshigh_uppertail_out    = colnames(censcore_uppertail_out)[order(censcore_uppertail_out[i,], decreasing = TRUE)][1:10]
  centrnodeslow_uppertail_in      = colnames(censcore_uppertail_in)[order(censcore_uppertail_in[i,], decreasing = FALSE)][1:10]
  centrnodeslow_uppertail_out     = colnames(censcore_uppertail_out)[order(censcore_uppertail_out[i,], decreasing = FALSE)][1:10]
 
  returns_uppertail[i,1]          = sum(DD[(312+i), centrnodeshigh_uppertail_in])/10
  returns_uppertail[i,2]          = sum(DD[(312+i), centrnodeshigh_uppertail_out])/10
  returns_uppertail[i,3]          = sum(DD[(312+i), centrnodeslow_uppertail_in])/10
  returns_uppertail[i,4]          = sum(DD[(312+i), centrnodeslow_uppertail_out])/10
  
}   


tradingret                        = cbind(returns_median, returns_lowertail, returns_uppertail)
write.csv(tradingret, file = "tradingreturns.csv")


# cumulative log excess returns
ff1                       = log(ff+1)
returns_median            = log(returns_median+1)
returns_lowertail         = log(returns_lowertail+1)
returns_uppertail         = log(returns_uppertail+1)
cumtradingmedian          = matrix(0,253,4)
for (i in 1:4){
  cumtradingmedian[,i]    = cumsum(returns_median[,i])
}
cumtradinglowertail       = matrix(0,253,4)
for (i in 1:4){
  cumtradinglowertail[,i] = cumsum(returns_lowertail[,i])
}
cumtradinguppertail       = matrix(0,253,4)
for (i in 1:4){
  cumtradinguppertail[,i] = cumsum(returns_uppertail[,i])
}
fac                       = matrix(0,253,2)
fac[,1]                   = cumsum(ff1[,1])
fac[,2]                   = cumsum(ff1[,5])
returnsdata               = cbind(cumtradingmedian,cumtradinglowertail,cumtradinguppertail,fac)
#for (i in 1:nrow(returnsdata)){
 # returnsdata[i,] = returnsdata[i,]^(12/i)-1
#}
for (i in 1:nrow(returnsdata)){
 returnsdata[i,]          = (12/i)*returnsdata[i,]
}
exreturns                 = cbind(returnsdata[,1:13]-returnsdata[,14])
rownames(exreturns)       = rownames(ff)
#time series plot of returns
#exreturns_ts = ts(exreturns, start=c(1996, 01), end=c(2017, 01), frequency=12)
#ts.plot(exreturns_ts, gpars = list(col=rainbow(10)))

#calculate means and t-stat
summret_LS                = matrix(0, 2, 13)
summret_LS[1,]            = colMeans(exreturns)
sds                       = apply(exreturns, 2, sd)
summret_LS[2,]            = (colMeans(exreturns))/sds

write.table(format(summret_LS, digits = 4), file = "summret_LS.txt", sep = "&amp;")
