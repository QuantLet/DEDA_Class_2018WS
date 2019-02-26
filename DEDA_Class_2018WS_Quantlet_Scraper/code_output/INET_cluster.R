#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# install and load packages
libraries = c("fields", "graphics", "stats")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

#setwd("/Users/qianya/Library/Mobile Documents/com~apple~CloudDocs/ffdata/test")

#Define the connectedness function
connectedness_cyc          = function(A) {
  W                        = A^(1/3)
  W1                       = W%*%W%*%W
  from                     = colSums(A!=0)
  to                       = rowSums(A!=0)
  A2                       = as.matrix((A&gt;0)+0)
  self                     = diag(A2%*%A2)
  conn_cyc                 = diag(W1)/(from*to-self)
  conn_cyc[is.na(conn_cyc)]= 0
  return(mean(conn_cyc))
}

connectedness_mid          = function(A) {
  W                        = A^(1/3)
  W2                       = W%*%t(W)%*%W
  from                     = colSums(A!=0)
  to                       = rowSums(A!=0)
  A2                       = as.matrix((A&gt;0)+0)
  self                     = diag(A2%*%A2)
  conn_mid                 = diag(W2)/(from*to-self)
  conn_mid[is.na(conn_mid)]= 0
  return(mean(conn_mid))
}

connectedness_ins          = function(A) {
  W                        = A^(1/3)
  W3                       = t(W)%*%W%*%W
  from                     = colSums(A!=0)
  conn_ins                 = diag(W3)/(from*(from-1))
  conn_ins[is.na(conn_ins)]= 0
  return(mean(conn_ins))
}

connectedness_out          = function(A) {
  W                        = A^(1/3)
  W4                       = W%*%W%*%t(W)
  to                       = rowSums(A!=0)
  conn_out                 = diag(W4)/(to*(to-1))
  conn_out[is.na(conn_out)]= 0
  return(mean(conn_out))
}

#whoel sample connectedness
datafile = c("beta_L_median.csv", "beta_L_lowertail.csv","beta_L_uppertail.csv")
cc = matrix(0,4,3)
colnames(cc) = c("median", "lowertail", "uppertail")
rownames(cc)  = c( "cycles", "middlemen", "ins", "out")
for (s in 1:length(datafile)){
  clustercoeff             = read.csv(datafile[s], header = TRUE, sep = ",", dec = ".")
  industrynames            = read.csv("industrynames.csv")
  clustercoeff             = clustercoeff[ ,-1]
  rownames(clustercoeff)   = industrynames[ ,2]
  colnames(clustercoeff)   = industrynames[ ,2]
  adjac                    = data.matrix(clustercoeff)
  diag(adjac)              = 0
  adjac                    = abs(adjac)
  adjac                    = t(adjac)
  cc[1,s]                  = connectedness_cyc(adjac)
  cc[2,s]                  = connectedness_mid(adjac)
  cc[3,s]                  = connectedness_ins(adjac)
  cc[4,s]                  = connectedness_out(adjac)
}
write.table(cc, file = "wholeconnectedness.txt", sep = '&amp;')

#moving window connectedness
datafile_mw                = c("mw/beta_L_median ", "mw/beta_L_lowertail ", "mw/beta_L_uppertail ")
cc_mw                      = matrix(0,177,12)
for (i in 1:length(datafile_mw)){
  for (s in 1:177){
    datafile               = paste(datafile_mw[i], s, " .csv", sep ="")
    clustercoeff           = read.csv(datafile, header = TRUE, sep = ",", dec = ".")
    clustercoeff           = clustercoeff[ ,-1]
    rownames(clustercoeff) = industrynames[ ,2]
    colnames(clustercoeff) = industrynames[ ,2]
    adjac                  = data.matrix(clustercoeff)
    diag(adjac)            = 0
    adjac                  = abs(adjac)
    adjac                  = t(adjac)
    cc_mw[s,(4*(i-1)+1)]   = connectedness_cyc(adjac)
    cc_mw[s,(4*(i-1)+2)]   = connectedness_mid(adjac)
    cc_mw[s,(4*(i-1)+3)]   = connectedness_ins(adjac)
    cc_mw[s,(4*(i-1)+4)]   = connectedness_out(adjac)
  }
}
#cc_mw_ave                  = colMeans(cc_mw)
#write.table(cc_mw_ave, file = "connectedness_mw.txt", sep = '&amp;')

colnames(cc_mw) = c('cyc_m','mid_m', 'ins_m', 'out_m', 'cyc_l','mid_l', 'ins_l', 'out_l', 'cyc_u','mid_u', 'ins_u', 'out_u')
dateindex = seq(as.Date("1970/1/1"), as.Date("2014/1/1"), "quarters")
cc_mw = as.data.frame(cc_mw)
write.csv(cc_mw, "cc_mw.csv")
png(file="ccplot_median.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$cyc_m, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$mid_m, col = 'red')
lines(dateindex, cc_mw$ins_m, col = 'green')
lines(dateindex, cc_mw$out_m, col = 'black')
dev.off()
png(file="ccplot_lowertail.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$cyc_l, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$mid_l, col = 'red')
lines(dateindex, cc_mw$ins_l, col = 'green')
lines(dateindex, cc_mw$out_l, col = 'black')
dev.off()
png(file="ccplot_uppertail.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$cyc_u, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$mid_u, col = 'red')
lines(dateindex, cc_mw$ins_u, col = 'green')
lines(dateindex, cc_mw$out_u, col = 'black')
dev.off()
png(file="cyclesplot.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$cyc_m, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$cyc_l, col = 'red')
lines(dateindex, cc_mw$cyc_u, col = 'green')
dev.off()
png(file="middlemenplot.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$mid_m, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$mid_l, col = 'red')
lines(dateindex, cc_mw$mid_u, col = 'green')
dev.off()
png(file="insplot.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$ins_m, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$ins_l, col = 'red')
lines(dateindex, cc_mw$ins_u, col = 'green')
dev.off()
png(file="outplot.png",width = 400, height=320, bg = NA)
plot(dateindex, cc_mw$out_m, col = 'blue', type ='l', ylim = c(0,0.25), ylab = 'clustering coefficient', xlab = 'date')
lines(dateindex, cc_mw$out_l, col = 'red')
lines(dateindex, cc_mw$out_u, col = 'green')
dev.off()
