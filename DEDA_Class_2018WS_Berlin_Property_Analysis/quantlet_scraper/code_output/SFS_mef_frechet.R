# ------------------------------------------------------------------------------ Book: SFS
# ------------------------------------------------------------------------------ Quantlet: SFS_mef_frechet
# ------------------------------------------------------------------------------ Description: SFS_mef_frechet plots the
# theoretical (red line) and empirical (blue) Mean excess function e(u) of the Frechet distribution with alpha = 2
# ------------------------------------------------------------------------------ Usage: -
# ------------------------------------------------------------------------------ Inputs: none
# ------------------------------------------------------------------------------ Output: Plot of mean excess function for
# Frechet distriubtion.  ------------------------------------------------------------------------------ Example: -
# ------------------------------------------------------------------------------ Author: Lasse Groth
# ------------------------------------------------------------------------------

# Close all plots and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# Install and load library
install.packages("matlab")
library(matlab)

# Simulate Frechet Distribution
nsim = 1000
urv = runif(nsim, min = 0, max = 1)
ones = matrix(1, nsim, 1)
rv = ones/((-log(urv))^(1/(2 * ones)))

k = 999

rv = sort(rv, decreasing = TRUE)
MEF = matrix(, , , )
t = rv[1:k + 1]  #t must be &gt;0

# Calculate the empirical mean excess function
for (i in 1:length(t)) {
    y = rv[rv &gt; t[i]]
    MEF[i] = mean(y - t[i])
}

plot(t[3:k + 1], t[3:k + 1], type = "l", col = "red", lwd = 2, xlab = c("u"), ylab = c("e(u)"))
points(t[3:k + 1], MEF[3:k + 1], col = "blue", pch = 23, bg = "blue")
title("Plot Mean Excess Function")
