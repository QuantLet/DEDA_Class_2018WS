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
tradingreturn              = read.csv("tradingreturns.csv", header = TRUE, sep = ",", dec = "." )
tradingreturn              = tradingreturn[,-1]
colnames(tradingreturn)    = c('HI_M','HO_M','TI_M','TO_M','HI_L','HO_L','TI_L','TO_L', 'HI_U','HO_U','TI_U','TO_U')
ff                         = read.csv("FF3factors.CSV", sep = "," )
ff                         = ff[(nrow(ff)-252):nrow(ff), ]
rownames(ff)               = ff[,1]
ff[,1]                     = ff[,2] + ff[,5]
ff                         = ff/100

exreturns                  = tradingreturn-ff[,5]
exreturnpanel              = cbind(exreturns, ff[,2])
returnpanel                = exreturnpanel+ff[,5]
#time series plot of returns
#exreturns_ts = ts(exreturns, start=c(1996, 01), end=c(2017, 01), frequency=12)
#ts.plot(exreturns_ts, gpars = list(col=rainbow(10)))

#calculate means and t-stat
summret_trading            = matrix(0, 2, 13)
summret_trading[1,]        = colMeans(exreturnpanel)
sds                        = apply(returnpanel, 2, sd)
summret_trading[2,]        = (colMeans(exreturnpanel))/sds

write.table(format(summret_trading, digits = 4), file = "summret_trading.txt", sep = "&amp;")
