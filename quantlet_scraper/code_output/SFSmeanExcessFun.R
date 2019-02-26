# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("matlab", "QRMlib")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


# load data
a = read.table("Bay9906_close_2kPoints.txt")
b = read.table("Bmw9906_close_2kPoints.txt")
c = read.table("Sie9906_close_2kPoints.txt")

d = a + b + c  # Create the portfolio
lg = dim(d)
x = log(d[-lg[1], ]) - log(d[-1, ])  # Negative log-returns

n = length(x)
x = sort(x, decreasing = TRUE)

m = 100
x1 = x[1:m]

# empirical mean excess function
t = x[1:(m + 1)]  # t must be &gt;0

MEF = matrix(, , , )

for (i in 1:length(t)) {
    y = x[find(x &gt; t[i])]
    MEF[i] = mean(y - t[i])
}

plot(t, MEF, type = "l", col = "blue", lwd = 3, ylim = c(0.005, 0.04), xlab = "u", 
    ylab = "e(u)")
title("Mean Excess Functions")

# mean excess function of generalized Pareto distribution, theorem 18.8
k = 100
GPD = fit.GPDb(x, nextremes = k, method = "ml", information = "observed")
K = GPD$par.ests[1]
sigma = GPD$par.ests[2]
gpme = (sigma + K * (t - mean(t)))/(1 - K)

lines(t, gpme, lwd = 3)

# Hill estimator, mean excess function of Pareto distribution
alphaH = (mean(log(x1)) - log(x1[k]))^(-1)
sigmaH = x1[k] * (k/n)^(1/alphaH)
gp1me = t/(alphaH - 1)

lines(t, gp1me, col = "red", lwd = 3, lty = 5)
