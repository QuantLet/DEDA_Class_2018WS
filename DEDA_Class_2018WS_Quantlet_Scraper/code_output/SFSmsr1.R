# ------------------------------------------------------------------------------ Book: SFS
# ------------------------------------------------------------------------------ Quantlet: SFSmsr1
# ------------------------------------------------------------------------------ Description: SFSmsr1 shows the rate of
# convergence to infinity for the stable distributed random variables is higher than for standard normal variables. Plots
# the convergence rate of maximum for n random variables with a standard normal cdf and with a 1.1-stable cdf.  Refers to
# exercise 16.2 in SFS.  ------------------------------------------------------------------------------ Usage: -
# ------------------------------------------------------------------------------ Inputs: None
# ------------------------------------------------------------------------------ Output: Plots of the convergence rate of
# maxima for standard normal cdf and 1.1-stable cdf.
# ------------------------------------------------------------------------------ Example: -
# ------------------------------------------------------------------------------ Author: Lasse Groth 20091006
# ------------------------------------------------------------------------------ install library
install.packages("QRM")

# Load library
library(QRM)

# Close all plots and clear variables
graphics.off()
rm(list = ls(all = TRUE))

n0 = 10
i = n0
n = 10000/n0
flag = 1

m = n/n0

dat1 = dat2 = matrix(, 100, 2)

for (i in 1:m) {
    
    dat1[i, 1] = i * 10
    dat1[i, 2] = max(rnorm(i, mean = 0, sd = 1))  #for normal distributions
    
    dat2[i, 1] = i * 10
    dat2[i, 2] = max(rstable(i, 1.1, 0))  #for stable distributions
}

plot(dat1[, 2], xlab = ("n"), ylab = ("M(n)"), col = "blue")
title("Limit of M(n) for normal cdf")

dev.new()
plot(dat2[, 2], xlab = ("n"), ylab = ("M(n)"), col = "blue")
title("Limit of M(n) for stable cdf")
