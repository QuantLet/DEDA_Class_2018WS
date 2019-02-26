# ------------------------------------------------------------------------------ Book: SFS
# ------------------------------------------------------------------------------ Quantlet: SFStailGEV
# ------------------------------------------------------------------------------ Description: SFStailGEV fits a Generalized
# Extreme Value Distribution to the negative log-returns of a portfolio (Bayer, BMW, Siemens) for the time period from
# 1992-01-01 to 2006-09-21 and produces a QQ-plot and a PP-plot.  Corresponds to exercise 16.5 in SFS.
# ------------------------------------------------------------------------------ Usage: -
# ------------------------------------------------------------------------------ Inputs: None
# ------------------------------------------------------------------------------ Output: QQ-plot and PP-plot with
# Generalized Extreme Value Distribution.  ------------------------------------------------------------------------------
# Example: - ------------------------------------------------------------------------------ Author: Lasse Groth 20091013
# ------------------------------------------------------------------------------

# Load library
install.packages("QRM")
library(QRM)

# Close all plots and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# Set working directory and load datasets
setwd("C:/...")

a = read.table("Bay9906_close_2kPoints.txt")
b = read.table("Bmw9906_close_2kPoints.txt")
c = read.table("Sie9906_close_2kPoints.txt")

d = a + b + c  #Create the portfolio
lg = dim(d)
x = log(d[-lg[1], ]) - log(d[-1, ])  #Negative log-return

# Determine the Block Maxima data
T = length(x)
n = 20
k = T/n
z = matrix(, , , )

for (j in 1:k) {
    
    r = x[((j - 1) * n + 1):(j * n)]
    z[j] = max(r)
}

w = sort(z)

GEV = fit.GEV(z)  #Fit the Generalized Extreme Value Distribution

K = GEV$par.ests[1]  #shape parameter
mu = GEV$par.ests[2]  #location parameter
sigma = GEV$par.ests[3]  #scale parameter

t = (1:k)/(k + 1)

y1 = qGEV(t, K, mu, sigma)
y2 = pGEV(w, K, mu, sigma)

# Plot the QQ plot
dev.new()
plot(w, y1, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(y1, y1, type = "l", col = "red", lwd = 2)
title("QQ plot, Generalized Extreme Value Distribution")

# Plot the PP plot
dev.new()
plot(y2, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(y2, y2, type = "l", col = "red", lwd = 2)
title("PP plot, Generalized Pareto Distribution")
