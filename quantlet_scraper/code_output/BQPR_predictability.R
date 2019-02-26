# clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("quantreg", "mvtnorm", "plyr", "KernSmooth", "rootSolve")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

setwd("~/Quantlet/Balanced_Quantile_Predictive_Regression/BQPR_predictability")

## Predictability tests by 2SQR(1)
TAU  = c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)
data = read.csv("Data0.csv", header = FALSE)
TT   = dim(data)[1]
data = data[1:TT, 1:9]  #from 1952 Jan (301) or 1927 Jan (1)
TT   = dim(data)[1]
y    = data[3:TT, 1]

## One predictor
stat2 = matrix(0, nrow = 8, ncol = length(TAU))
for (pp in 1:8) {
    XX    = cbind(data[3:TT, (pp + 1)], data[2:(TT - 1), (pp + 1)])
    ehat  = lm(data[3:TT, (pp + 1)] ~ 0 + data[2:(TT - 1), (pp + 1)])$residuals
    beta2 = matrix(0, ncol = 2, nrow = length(TAU))
    for (t in 1:length(TAU)) {
        fit   = rq(formula = y ~ 1 + XX, tau = TAU[t])
        beta  = coef(fit)
        y.tau = y - cbind(rep(1, length(y)), ehat) %*% beta[1:2]
        XX2   = cbind(data[2:(TT - 1), (pp + 1)], data[1:(TT - 2), (pp + 1)])
        fit2  = rq(formula = y.tau ~ 0 + XX2, tau = TAU[t])
        BETA  = coef(fit2)
        error = y.tau - XX2 %*% BETA
        ind   = which(density(error)$x == min(abs(density(error)$x)))
        if (length(ind) == 0) {
            f.u_tau = density(error)$y[which(density(error)$x == -min(abs(density(error)$x)))]
        } else {
            f.u_tau = density(error)$y[which(density(error)$x == min(abs(density(error)$x)))]
        }
        beta2[t, ]   = c(BETA[[1]], BETA[[2]])
        stat2[pp, t] = sqrt(TT - 2) * f.u_tau * sqrt(ehat %*% ehat/(TT - 3)) * (beta2[t, 
            1])/sqrt(TAU[t] * (1 - TAU[t]))
    }
}
1 - pchisq(stat2^2, 1)

## Predictability tests by 2SQR(2)
TAU  = c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)
data = read.csv("Data0.csv", header = FALSE)
TT   = dim(data)[1]
data = data[1:TT, 1:9]  #from 1952 Jan (301) or 1927 Jan (1)
TT   = dim(data)[1]
y    = data[2:TT, 1]

## One predictor
stat2 = matrix(0, nrow = 8, ncol = length(TAU))
for (pp in 1:8) {
    XX    = cbind(data[2:TT, (pp + 1)], data[1:(TT - 1), (pp + 1)])
    X     = data[1:(TT - 1), (pp + 1)]
    ehat  = lm(data[2:TT, (pp + 1)] ~ 0 + data[1:(TT - 1), (pp + 1)])$residuals
    beta2 = rep(0, length(TAU))
    for (t in 1:length(TAU)) {
        fit   = rq(formula = y ~ 1 + XX, tau = TAU[t])
        beta  = coef(fit)
        y.tau = y - cbind(rep(1, length(y)), ehat) %*% beta[1:2]
        fit2  = rq(formula = y.tau ~ 0 + X, tau = TAU[t])
        BETA  = coef(fit2)
        error = y.tau - X * BETA
        ind   = which(density(error)$x == min(abs(density(error)$x)))
        if (length(ind) == 0) {
            f.u_tau = density(error)$y[which(density(error)$x == -min(abs(density(error)$x)))]
        } else {
            f.u_tau = density(error)$y[which(density(error)$x == min(abs(density(error)$x)))]
        }
        beta2[t]     = BETA
        stat2[pp, t] = f.u_tau^2 * (beta2[t]) * (data[2:TT, (pp + 1)] %*% data[2:TT, 
            (pp + 1)] - sum(data[2:TT, (pp + 1)])^2/(TT - 1)) * (beta2[t])/(TAU[t] * 
            (1 - TAU[t]))
    }
}
1 - pchisq(stat2, 1)

## Two predictors
stat2 = matrix(0, nrow = 3, ncol = length(TAU))
R     = diag(2)
pp2   = 8
for (ppp in 1:length(c(2, 4, 5))) {
    pp    = c(2, 4, 5)[ppp]
    XX    = as.matrix(cbind(data[2:TT, c((pp + 1), pp2)], data[1:(TT - 1), c((pp + 1), 
           pp2)]))
    beta2 = matrix(0, ncol = 2, nrow = length(TAU))
    for (t in 1:length(TAU)) {
        fit   = rq(formula = y ~ 1 + XX, tau = TAU[t])
        beta  = coef(fit)
        ehat  = cbind(lm(data[2:TT, (pp + 1)] ~ 0 + data[1:(TT - 1), (pp + 1)])$residuals, 
             lm(data[2:TT, pp2] ~ 0 + data[1:(TT - 1), pp2])$residuals)
        y.tau = y - cbind(rep(1, length(y)), ehat) %*% beta[1:3]
        X     = as.matrix(data[1:(TT - 1), c((pp + 1), pp2)])
        fit2  = rq(formula = y.tau ~ 0 + X, tau = TAU[t])
        BETA  = coef(fit2)
        error = y.tau - X %*% BETA
        ind   = which(density(error)$x == min(abs(density(error)$x)))
        if (length(ind) == 0) {
            f.u_tau = density(error)$y[which(density(error)$x == -min(abs(density(error)$x)))]
        } else {
            f.u_tau = density(error)$y[which(density(error)$x == min(abs(density(error)$x)))]
        }
        beta2[t, ]    = BETA
        stat2[ppp, t] = f.u_tau^2 * (beta2[t, ]) %*% (t(as.matrix(data[2:TT, c((pp + 
            1), pp2)])) %*% as.matrix(data[2:TT, c((pp + 1), pp2)]) - colSums(as.matrix(data[2:TT, 
            c((pp + 1), pp2)])) %*% t(colSums(as.matrix(data[2:TT, c((pp + 1), pp2)])))/(TT - 
            1)) %*% (beta2[t, ])/(TAU[t] * (1 - TAU[t]))
    }
}
1 - pchisq(stat2, 2)
