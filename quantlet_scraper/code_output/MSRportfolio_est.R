rm(list = ls(all = TRUE))
#setwd("C:/...")

#install.packages("KernSmooth")
library(KernSmooth)

portRet  =  read.table("Port9906_2kPoints_GARCHn_Inn.txt")
portRet  =  as.matrix(portRet)

dim(portRet)
r   =  length(portRet)

mu  =  mean(portRet)
si  =  sqrt(var(portRet))

set.seed(3)

x  =  si * rnorm(r) + mu
h  =  1.06 * si * (r^(-0.2))

par(mfrow = c(1, 2))

x1  =  bkde(portRet, bandwidth = h)
x2  =  bkde(x, bandwidth = h)

plot(x1, type = "l", col = "red", xlab = "", ylab = "", 
    main = "Estimated Density (Nonparametric)", 
    cex.main = 0.9)
lines(x2, col = "blue", lty = 2)

lfh  =  cbind(x1$x, log(x1$y))
lf   =  cbind(x2$x, log(x2$y))

plot(lfh, type = "l", col = "red", xlab = "", ylab = "", 
    main = "Estimated Log-Density (Nonparametric)", 
    cex.main = 0.9) 
lines(lf, col = "blue", lty = 2)
