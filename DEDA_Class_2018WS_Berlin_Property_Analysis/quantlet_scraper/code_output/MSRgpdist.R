rm(list=ls(all=TRUE))
#setwd("C:/...")

#install.packages("QRM")
library(QRM)
 
n     = 100
sp    = 5 
r     = c(0.5, 0, -0.5)
 
x1    = seq(0.001, 5.001, by = 0.05) 
dist1 = cbind(x1, dGPD(x1, r[1], 1))

x2    = c(0, sp*(1:n)/n)
dist2 = cbind(x2, dGPD(x2, r[2], 1))

x3    = seq(0.001, 100*(-1/r[3]/n)+0.001, -1/r[3]/n)
dist3 = cbind(x3, dGPD(x3, r[3],1))

plot(dist1, type = "l", col = "blue", lty = 4, lwd = 3, xlab = "", ylab = "", 
    main = "Generalized Pareto Densities")
lines(dist2, lwd = 3)
lines(dist3, col = "red", lty = 2, lwd = 3)
