graphics.off()
rm(list = ls(all = TRUE))
# setwd('C:/...')

# install.packages('fBasics') install.packages('stabledist')

library(fBasics)
library(stabledist)
x &lt;- c(-50:50)/11

alpha &lt;- c(0.5, 0.75, 1, 1.25, 1.5)
beta &lt;- 0.5

# S parameterization
w1 &lt;- dstable(x, alpha[1], beta = beta, pm = 1)
w2 &lt;- dstable(x, alpha[2], beta = beta, pm = 1)
w3 &lt;- dstable(x, alpha[3], beta = beta, pm = 1)
w4 &lt;- dstable(x, alpha[4], beta = beta, pm = 1)
w5 &lt;- dstable(x, alpha[5], beta = beta, pm = 1)

plot(x, w1, type = "l", main = "S parameterization", xlab = "x", ylab = "PDF(x)", cex.axis = 2, cex.lab = 1.4, cex.main = 2, 
    lwd = 3)
lines(x, w2, col = "red", lwd = 3, lty = 3)
lines(x, w3, col = "blue", lwd = 3, lty = 2)
lines(x, w4, col = "green", lwd = 3, lty = 5)
lines(x, w5, col = "cyan", lwd = 3, lty = 4)

################################## 

# S0 parameterization
mu2 &lt;- -beta * tan(0.5 * pi * alpha)
mu2[3] = 0

w6 &lt;- dstable(x, alpha[1], beta = beta, pm = mu2[1])
w7 &lt;- dstable(x, alpha[2], beta = beta, pm = mu2[2])
w8 &lt;- dstable(x, alpha[3], beta = beta, pm = mu2[3])
w9 &lt;- dstable(x, alpha[4], beta = beta, pm = mu2[4])
w10 &lt;- dstable(x, alpha[5], beta = beta, pm = mu2[5])
dev.new()
plot(x, w6, type = "l", main = "S0 parameterization", xlab = "x", ylab = "PDF(x)", cex.axis = 2, cex.lab = 1.4, cex.main = 2, 
    lwd = 3)
lines(x, w7, col = "red", lwd = 3, lty = 3)
lines(x, w8, col = "blue", lwd = 3, lty = 2)
lines(x, w9, col = "green", lwd = 3, lty = 5)
lines(x, w10, col = "cyan", lwd = 3, lty = 4)
