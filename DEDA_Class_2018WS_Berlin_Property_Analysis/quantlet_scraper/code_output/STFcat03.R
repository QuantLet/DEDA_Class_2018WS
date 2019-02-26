rm(list = ls(all = TRUE))
# setwd('C:/...')

m &lt;- read.table("ncl.dat")

lambdaHPP &lt;- 34.2
lambdaNHPP1 &lt;- c(35.32, 2.32 * 2 * pi, -0.2)
lambdaNHPP2 &lt;- c(35.22, 0.224, -0.16)

A &lt;- 4
B &lt;- 6
Delta &lt;- 0.01

n &lt;- table(m[, 5])
n &lt;- as.data.frame(n)$Freq
t &lt;- (1:length(n))/4
tn &lt;- cbind(t, n)
ncum &lt;- cumsum(n)
tncum &lt;- cbind(t, ncum)

t &lt;- seq(A, B, Delta)

HP &lt;- lambdaHPP * t
NHP1 &lt;- lambdaNHPP1[1] * t - lambdaNHPP1[2]/(2 * pi) * cos(2 * pi * (t + lambdaNHPP1[3])) + lambdaNHPP1[2]/(2 * pi) * cos(2 * 
    pi * lambdaNHPP1[3])
NHP2 &lt;- lambdaNHPP2[1] * t + lambdaNHPP2[2] * (0.5 * t - 1/(8 * pi) * sin(4 * pi * (t + lambdaNHPP2[3])) + 1/(8 * pi) * sin(4 * 
    pi * lambdaNHPP2[3]))

plot(tncum[16:24, ], type = "l", col = "blue", lwd = 3, xlim = c(4, 6), ylim = c(139, 219), xlab = "Years", ylab = "Aggregate number of losses / Mean value function", 
    cex.lab = 1.4, cex.axis = 1.4)
lines(t, HP, col = "red", lwd = 3, lty = 3)
lines(t, NHP1, col = "black", lwd = 3, lty = 2)
lines(t, NHP2, col = "green", lwd = 3, lty = 4)
