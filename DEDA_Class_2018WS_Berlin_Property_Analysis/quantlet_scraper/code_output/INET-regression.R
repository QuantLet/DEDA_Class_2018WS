#Close windows and clear variables                                                                   
graphics.off()
rm(list = ls(all=TRUE))

# install and load packages
libraries = c("stats")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# read market factor and firm characteristics
fffactors                 = read.csv("F-F_3Factors.csv", header = TRUE, sep = ",", dec = ".")
rownames(fffactors)       = fffactors[,1]
fffactors[,1]             = fffactors[,2]+fffactors[,5]
fffactors                 = fffactors[,c(1,3,4)]
factorvol                 = matrix(0,177,3)
for (i in 1:177){
  for (j in 1:3)
    factorvol[i,j]        = sd(fffactors[((3*i-2):(3*i+33)),j])
}
factorvol                 = as.data.frame(factorvol)

totalconn                 = read.csv("conn_mw.csv", header = TRUE, sep = ",", dec = ".")
rownames(totalconn)       = totalconn[,1]
totalconn                 = totalconn[,-1]
for (i in 1:3){
  lm_reg                  = lm(clustering[,i]~ factorvol[,1]+factorvol[,2]+factorvol[,3])
  summary(lm_reg)
}


clustering                = read.csv("cc_mw.csv", header = TRUE, sep = ",", dec = ".")
clustering                = clustering[,-1]
rownames(clustering)      = rownames(totalconn)
for (j in 1:12){
  lm_reg                  = lm(clustering[,j]~ factorvol[,1]+factorvol[,2]+factorvol[,3])
  summary(lm_reg)
}
