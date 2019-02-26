## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("quantreg", "mvtnorm", "plyr", "KernSmooth", "rootSolve")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## 2SQR(1)
n      = ceiling(268 * 5/4)  # the sample size n=250, discard the first quarter
ns     = ceiling(n/4)
nsim   = 1000  # the times of repeats in simulation
TAU    = 0.5
c      = c(5, 25, 40)
r      = c(1, 1, 1)
a      = 1 - c/(n - ns - 1)^r
b0     = 0
nu     = c(1, 2, 3)
b2     = seq(0, 0.3, length.out = 20)
b1     = 0.3
cov    = 0
sig.e  = c(1, 1, 1)
sig.u  = 1
beta2  = matrix(0, ncol = 2, nrow = nsim)
stat2  = rep(0, nsim)
mu     = rep(0, 2)
reject = list()
set.seed(2016)
for (t in 1:length(TAU)) {
    reject[[t]] = matrix(0, nrow = length(a), ncol = length(b2))
    for (k in 1:length(a)) {
        for (p in 1:length(b2)) {
            for (i in 1:nsim) {
                sigma  = cbind(c(sig.u, cov), c(cov, sig.e[k]))
                # innov = rmvnorm(n, mu, sigma) # normal errors
                scale  = sigma * nu[3]/(nu[3] - 2)  # t errors
                innov  = rmvt(n, sigma = scale, df = nu[3])
                e      = innov[, 2]
                u      = innov[, 1]
                x      = rep(0, n)
                x[1]   = e[1]
                for (j in 2:n) {
                  x[j] = a[k] * x[j - 1] + e[j]
                }
                X     = x[(ns + 1):(n - 1)]
                b     = b2[p]
                y     = b0 + X * b + u[(ns + 2):n] + b1 * e[(ns + 2):n]
                ehat  = lm(x[(ns + 2):n] ~ 0 + x[(ns + 1):(n - 1)])$residuals
                XX    = cbind(x[(ns + 2):n], X)
                fit   = rq(formula = y ~ 1 + XX, tau = TAU[t])
                beta  = coef(fit)
                y.tau = y - cbind(rep(1, length(y)), ehat) %*% beta[1:2]
                XX2   = cbind(X, x[ns:(n - 2)])
                fit2  = rq(formula = y.tau ~ 0 + XX2, tau = TAU[t])
                BETA  = coef(fit2)
                error = y.tau - XX2 %*% BETA
                ind   = which(density(error)$x == min(abs(density(error)$x)))
                if (length(ind) == 0) {
                  f.u_tau = density(error)$y[which(density(error)$x == -min(abs(density(error)$x)))]
                } else {
                  f.u_tau = density(error)$y[which(density(error)$x == min(abs(density(error)$x)))]
                }
                beta2[i, ] = c(BETA[[1]], BETA[[2]])
                stat2[i]   = sqrt(n - ns - 1) * f.u_tau * sqrt(ehat %*% ehat/(n - ns - 
                  2)) * (beta2[i, 1])/sqrt(TAU[t] * (1 - TAU[t]))
                if (sign(pnorm(abs(stat2[i])) - 0.975) == 1) {
                  reject[[t]][k, p] = reject[[t]][k, p] + 1
                }
            }
        }
    }
    reject[[t]] = reject[[t]]/nsim
}

reject.t = reject  #save the results of 2SQR(1)

## 2SQR(2)
n      = ceiling(268 * 5/4)  # the sample size n=250 or 700, discard the first quarter
ns     = ceiling(n/4)
nsim   = 1000  # the times of repeats in simulation
TAU    = 0.5
c      = c(5, 25, 40)
r      = c(1, 1, 1)
a      = 1 - c/(n - ns - 1)^r
b0     = 0
nu     = c(1, 2, 3)
b2     = seq(0, 0.3, length.out = 20)
b1     = 0.3
cov    = 0
sig.e  = c(1, 1, 1)
sig.u  = 1
beta2  = rep(0, nsim)
stat2  = rep(0, nsim)
mu     = rep(0, 2)
reject = list()
set.seed(2016)
for (t in 1:length(TAU)) {
    reject[[t]] = matrix(0, nrow = length(a), ncol = length(b2))
    for (k in 1:length(a)) {
        for (p in 1:length(b2)) {
            for (i in 1:nsim) {
                sigma  = cbind(c(sig.u, cov), c(cov, sig.e[k]))
                # innov = rmvnorm(n, mu, sigma) # normal errors
                scale  = sigma * nu[3]/(nu[3] - 2)  # t errors
                innov  = rmvt(n, sigma = scale, df = nu[3])
                e      = innov[, 2]
                u      = innov[, 1]
                x      = rep(0, n)
                x[1]   = e[1]
                for (j in 2:n) {
                  x[j] = a[k] * x[j - 1] + e[j]
                }
                X      = x[(ns + 1):(n - 1)]
                b      = b2[p]
                y      = b0 + X * b + u[(ns + 2):n] + b1 * e[(ns + 2):n]
                ehat   = lm(x[(ns + 2):n] ~ 0 + x[(ns + 1):(n - 1)])$residuals
                XX     = cbind(x[(ns + 2):n], X)
                fit2   = rq(formula = y ~ 1 + XX, tau = TAU[t])
                BETA2  = coef(fit2)
                y.tau2 = y - cbind(rep(1, length(y)), ehat) %*% c(BETA2[1], BETA2[2])
                fit3   = rq(formula = y.tau2 ~ 0 + X, tau = TAU[t])
                BETA3  = coef(fit3)
                error  = residuals(fit3)
                ind    = which(density(error)$x == min(abs(density(error)$x)))
                if (length(ind) == 0) {
                  f.u_tau = density(error)$y[which(density(error)$x == -min(abs(density(error)$x)))]
                } else {
                  f.u_tau = density(error)$y[which(density(error)$x == min(abs(density(error)$x)))]
                }
                beta2[i]  = BETA3[[1]]
                stat2[i]  = f.u_tau^2 * (beta2[i]) * (x[(ns + 2):n] %*% x[(ns + 2):n] - 
                  sum(x[(ns + 2):n])^2/(n - ns - 1)) * (beta2[i])/(TAU[t] * (1 - 
                  TAU[t]))
                if (sign(pchisq(stat2[i], 1) - 0.95) == 1) {
                  reject[[t]][k, p] = reject[[t]][k, p] + 1
                }
            }
        }
    }
    reject[[t]] = reject[[t]]/nsim
}

reject.sm = reject  #save the results of 2SQR(2)

setwd("~/Quantlet/Balanced_Quantile_Predictive_Regression/BQPR_power")
power = as.matrix(read.csv2("power_Lee.csv", sep = ";", header = FALSE))  #load the results of IVX-QR

par(mfrow = c(1, 3), mgp = c(1.5, 0.5, 0), oma = c(1, 1, 1, 1), mar = c(2.5, 1.8, 
    1, 1), xpd = NA, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
plot(b2, reject.t[[1]][1, ], xlab = expression(beta[paste("2, ", "0.5")]), ylab = "Power", 
    type = "l", ylim = c(0, 1), lwd = 3)
lines(b2, power[3, 1:20], lwd = 3, lty = 2)
lines(b2, reject.sm[[1]][1, ], lwd = 3, lty = 3)
title(main = "c=-5, normal", line = 0.2)
plot(b2, reject.t[[1]][2, ], xlab = expression(beta[paste("2, ", "0.5")]), ylab = "Power", 
    type = "l", ylim = c(0, 1), lwd = 3)
lines(b2, power[4, 1:20], lwd = 3, lty = 2)
lines(b2, reject.sm[[1]][2, ], lwd = 3, lty = 3)
title(main = "c=-25, normal", line = 0.2)
plot(b2, reject.t[[1]][3, ], xlab = expression(beta[paste("2, ", "0.5")]), ylab = "Power", 
    type = "l", ylim = c(0, 1), lwd = 3)
lines(b2, power[5, 1:20], lwd = 3, lty = 2)
lines(b2, reject.sm[[1]][3, ], lwd = 3, lty = 3)
title(main = "c=-40, normal", line = 0.2)

par(mfrow = c(1, 3), mgp = c(1.5, 0.5, 0), oma = c(1, 1, 1, 1), mar = c(2.5, 1.8, 
    1, 1), xpd = NA, cex.axis = 1.5, cex.lab = 1.5, cex.main = 2)
plot(b2, reject.t[[1]][1, ], xlab = expression(beta[paste("2, ", "0.5")]), ylab = "Power", 
    type = "l", ylim = c(0, 1), lwd = 3)
lines(b2, power[7, 1:20], lwd = 3, lty = 2)
lines(b2, reject.sm[[1]][1, ], lwd = 3, lty = 3)
title(main = "c=-5, t(3)", line = 0.2)
plot(b2, reject.t[[1]][2, ], xlab = expression(beta[paste("2, ", "0.5")]), ylab = "Power", 
    type = "l", ylim = c(0, 1), lwd = 3)
lines(b2, power[8, 1:20], lwd = 3, lty = 2)
lines(b2, reject.sm[[1]][1, ], lwd = 3, lty = 3)
title(main = "c=-25, t(3)", line = 0.2)
plot(b2, reject.t[[1]][3, ], xlab = expression(beta[paste("2, ", "0.5")]), ylab = "Power", 
    type = "l", ylim = c(0, 1), lwd = 3)
lines(b2, power[9, 1:20], lwd = 3, lty = 2)
lines(b2, reject.sm[[1]][1, ], lwd = 3, lty = 3)
title(main = "c=-40, t(3)", line = 0.2)
