rm(list = ls(all = TRUE))
#setwd("C:/...")

x    = read.table("Portf9206_logRet.dat")

n    = nrow(x)
xf   = apply(x, 2, sort)  

t    = (1:n)/(n+1);
dat1 = cbind(pnorm((xf - mean(xf))/sd(xf)), t)
dat2 = cbind(t, t)

#PP Plot
plot(dat1, col = "blue", ylab = "", xlab = "", main = "PP Plot of Daily Return of Portfolio")
lines(dat2, col = "red", lwd = 2)

#QQ Plot
qqnorm(xf, col = "blue", xlab = "", ylab = "", main = "QQ Plot of Daily Return of Portfolio")
qqline(xf, col = "red", lwd = 2)
