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
n      = ceiling(748 * 5/4)  # the sample size n=250, discard the first quarter
ns     = ceiling(n/4)
nsim   = 1000  # the times of repeats in simulation
TAU    = c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)
c      = c(0.1, 0.6, 0, 0.1, -0.3)
r      = c(0.9, 1, 1, 0, 0.6)
a      = 1 - c/(n - ns - 1)^r
b0     = 0
nu     = c(1, 2, 3)
b2     = 0
b1     = 0.3
cov    = 0
sig.e  = c(1, 1, 1, 1, 1)
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
        innov = rmvnorm(n, mu, sigma) # normal errors
        # scale  = sigma * nu[3]/(nu[3] - 2)  # t errors
        # innov  = rmvt(n, sigma = scale, df = nu[3])
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

REJECT = 0
for(t in 1:length(TAU)){
  REJECT = cbind(REJECT, reject[[t]])
}
REJECT = REJECT[, -1]
REJECT

## 2SQR(2)
n      = ceiling(748 * 5/4)  # the sample size n=250 or 700, discard the first quarter
ns     = ceiling(n/4)
nsim   = 1000  # the times of repeats in simulation
TAU    = c(0.05, seq(0.1, 0.9, by = 0.1), 0.95)
c      = c(0.1, 0.6, 0, 0.1, -0.3)
r      = c(0.9, 1, 1, 0, 0.6)
a      = 1 - c/(n - ns - 1)^r
b0     = 0
nu     = c(1, 2, 3)
b2     = 0
b1     = 0.3
cov    = 0
sig.e  = c(1, 1, 1, 1, 1)
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
        innov = rmvnorm(n, mu, sigma) # normal errors
        # scale  = sigma * nu[3]/(nu[3] - 2)  # t errors
        # innov  = rmvt(n, sigma = scale, df = nu[3])
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

REJECT = 0
for(t in 1:length(TAU)){
  REJECT = cbind(REJECT, reject[[t]])
}
REJECT = REJECT[, -1]
REJECT
