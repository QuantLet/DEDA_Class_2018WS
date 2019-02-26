## clear history
rm(list = ls(all = TRUE))
graphics.off()

## install and load packages
libraries = c("quantreg", "mvtnorm", "plyr", "KernSmooth", "rootSolve")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

## rolling window backtesting
setwd("~/Quantlet/Balanced_Quantile_Predictive_Regression/BQPR_backtesting")
date    = read.csv("Data0_withVariablesName.csv", sep = ";")[, 1]
date    = date[which(date == 192701):length(date)]
data    = read.csv("Data0.csv", header = FALSE)
TT      = dim(data)[1]
data    = data[1:TT, 1:9]  #from 1952 Jan (301) or 1927 Jan (1)
TT      = dim(data)[1]
h       = 1
rwindow = 300
## one predictor
TAU     = c(0.05, 0.5, 0.95)
frequn  = rep(0, 8)
frequn1 = rep(0, 8)
frequn3 = rep(0, 8)
qhat    = list()
for (v in 1:8) {
    qhat[[v]] = matrix(0, nrow = TT - rwindow, ncol = length(TAU))
}
outy = rep(0, TT - rwindow)
for (rw in 1:(TT - rwindow)) {
    sample   = data[rw:(rw + rwindow), ]
    TTT      = dim(sample)[1]
    y        = sample[2:(TTT - h), 1]
    outy[rw] = sample[TTT, 1]
    for (v in (1:8)) {
        XX   = cbind(sample[2:(TTT - h), (v + 1)], sample[1:(TTT - 1 - h), (v + 1)])
        ehat = lm(sample[2:(TTT - h), (v + 1)] ~ 0 + sample[1:(TTT - 1 - h), (v + 
            1)])$residuals
        ehat = c(ehat, (sample[TTT, (v + 1)] - lm(sample[2:(TTT - h), (v + 1)] ~ 
            0 + sample[1:(TTT - 1 - h), (v + 1)])$coefficients * sample[(TTT - 1), 
            (v + 1)]))
        for (t in 1:length(TAU)) {
            fit   = rq(formula = y ~ 1 + XX, tau = TAU[t])
            beta  = coef(fit)
            y.tau = y - cbind(rep(1, length(y)), ehat[1:(length(ehat) - 1)]) %*% 
                beta[1:2]
            X     = sample[1:(TTT - 1 - h), (v + 1)]
            fit2  = rq(formula = y.tau ~ 0 + X, tau = TAU[t])
            BETA  = coef(fit2)
            qhat[[v]][rw, t] = c(1, ehat[length(ehat)], sample[(TTT - 1), (v + 1)]) %*% 
                c(beta[1:2], BETA)
        }
        if (outy[rw] &gt;= qhat[[v]][rw, 3] || outy[rw] &lt;= qhat[[v]][rw, 1]) {
            frequn[v] = frequn[v] + 1
        }
        if (outy[rw]  &gt;= qhat[[v]][rw, 3]) {
            frequn3[v] = frequn3[v] + 1
        }
        if (outy[rw]  &lt;= qhat[[v]][rw, 1]) {
            frequn1[v] = frequn1[v] + 1
        }
    }
}
frequn /(TT - rwindow)
frequn1/(TT - rwindow)
frequn3/(TT - rwindow)

titles = c("d/p", "e/p", "b/m", "ntis", "d/e", "tbl", "dfy", "tms")
par(mfrow = c(2, 1), mgp = c(1.7, 0.5, 0), oma = c(1, 1, 1, 1), mar = c(2.8, 1.8, 
    1, 1), xpd = NA, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
for (v in c(5, 7)) {
    plot(qhat[[v]][, 1], type = "l", axes = FALSE, xlab = "dates", ylab = " ", ylim = c(-0.3, 
        0.25), col = "blue", lwd = 3)
    axis(1, at = seq(1, length(outy), length.out = 5), labels = date[seq(rwindow + 
        1, rwindow + length(outy), length.out = 5)])
    axis(2, at = seq(-0.3, 0.25, by = 0.1), labels = seq(-0.3, 0.25, by = 0.1))
    box()
    points(outy, pch = 20, cex = 0.8)
    lines(qhat[[v]][, 3], col = "red", lwd = 3)
    title(main = titles[v])
}

## two predictors
TAU     = c(0.05, 0.5, 0.95)
frequn  = rep(0, 3)
frequn1 = rep(0, 3)
frequn3 = rep(0, 3)
qhat    = list()
for (v in 1:3) {
    qhat[[v]] = matrix(0, nrow = TT - rwindow, ncol = length(TAU))
}
outy = rep(0, TT - rwindow)
for (rw in 1:(TT - rwindow)) {
    sample   = data[rw:(rw + rwindow), ]
    TTT      = dim(sample)[1]
    y        = sample[2:(TTT - h), 1]
    outy[rw] = sample[TTT, 1]
    pp2      = 8
    for (ppp in 1:length(c(2, 4, 5))) {
        pp = c(2, 4, 5)[ppp]
        XX = as.matrix(cbind(sample[2:(TTT - h), c((pp + 1), pp2)], sample[1:(TTT - 
            1 - h), c((pp + 1), pp2)]))
        for (t in 1:length(TAU)) {
            fit   = rq(formula = y ~ 1 + XX, tau = TAU[t])
            beta  = coef(fit)
            ehat  = cbind(lm(sample[2:(TTT - h), (pp + 1)] ~ 0 + sample[1:(TTT - 1 - 
                h), (pp + 1)])$residuals, lm(sample[2:(TTT - h), pp2] ~ 0 + sample[1:(TTT - 
                1 - h), pp2])$residuals)
            ehat  = rbind(ehat, c((sample[TTT, (pp + 1)] - lm(sample[2:(TTT - h), 
                (pp + 1)] ~ 0 + sample[1:(TTT - 1 - h), (pp + 1)])$coefficients * 
                sample[(TTT - 1), (pp + 1)]), (sample[TTT, pp2] - lm(sample[2:(TTT - 
                h), pp2] ~ 0 + sample[1:(TTT - 1 - h), pp2])$coefficients * sample[(TTT - 
                1), pp2])))
            y.tau = y - cbind(rep(1, length(y)), ehat[1:(dim(ehat)[1] - 1), ]) %*% 
                beta[1:3]
            X     = as.matrix(sample[1:(TTT - 1 - h), c((pp + 1), pp2)])
            fit2  = rq(formula = y.tau ~ 0 + X, tau = TAU[t])
            BETA  = coef(fit2)
            qhat[[ppp]][rw, t] = as.numeric(c(1, ehat[dim(ehat)[1], ], sample[(TTT - 
                1), c((pp + 1), pp2)])) %*% c(beta[1:3], BETA)
        }
        if (outy[rw] &gt;= qhat[[ppp]][rw, 3] || outy[rw] &lt;= qhat[[ppp]][rw, 1]) {
            frequn[ppp] = frequn[ppp] + 1
        }
        if (outy[rw] &gt;= qhat[[ppp]][rw, 3]) {
            frequn3[ppp] = frequn3[ppp] + 1
        }
        if (outy[rw] &lt;= qhat[[ppp]][rw, 1]) {
            frequn1[ppp] = frequn1[ppp] + 1
        }
    }
}
frequn /(TT - rwindow)
frequn1/(TT - rwindow)
frequn3/(TT - rwindow)

titles = c("e/p, dfy", "ntis, dfy", "d/e, dfy")
par(mfrow = c(2, 1), mgp = c(1.7, 0.5, 0), oma = c(1, 1, 1, 1), mar = c(2.8, 1.8, 
    1, 1), xpd = NA, cex.axis = 1.5, cex.lab = 1.5, cex.main = 1.5)
for (v in 2:3) {
    plot(qhat[[v]][, 1], type = "l", axes = FALSE, xlab = "dates", ylab = " ", ylim = c(-0.32, 
        0.32), col = "blue", lwd = 3)
    axis(1, at = seq(1, length(outy), length.out = 5), labels = date[seq(rwindow + 
        1, rwindow + length(outy), length.out = 5)])
    axis(2, at = seq(-0.32, 0.32, by = 0.08), labels = seq(-0.32, 0.32, by = 0.08))
    box()
    points(outy, pch = 20, cex = 0.8)
    lines(qhat[[v]][, 3], col = "red", lwd = 3)
    title(main = titles[v])
}
