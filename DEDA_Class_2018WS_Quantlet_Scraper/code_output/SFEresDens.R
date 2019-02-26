# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("copula", "fGarch", "fBasics")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
S = read.table("fx_dem_gbp_usd.dat")

# Log returns
X = S[-1, ]
for (i in 1:length(S[1, ])) X[, i] = log(S[-1, i]/S[-length(S[, i]), i])
eps = X
sigma.t = eps

# The fit of a GARCH(1,1) model to the sample of log returns
for (i in 1:length(S[1, ])) {
    fit = garchFit(~garch(1, 1), data = X[, i], trace = F)
    eps[, i] = fit@residuals/fit@sigma.t
}

# Determine mean and standard deviation
means = mean(eps)
sds = sd(eps)

# Kernel density estimator of the residuals and of the normal density
for (i in 1:dim(S)[2]) {
    win.graph()
    plot(density(eps[, i], kernel = "biweight"), main = "", xlab = "X", ylab = "Y", 
        col = "blue3", lwd = 3, cex.axis = 1.5, cex.lab = 1.5)
    x.norm = seq(means[i] - 6 * sds[i], means[i] + 6 * sds[i], length.out = 100)
    y.norm = dnorm(x.norm, means[i], sds[i])
    lines(x.norm, y.norm, col = "red3", type = "l", lwd = 3)
}
