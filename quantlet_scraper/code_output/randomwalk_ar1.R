N			= 50
StartVal	= 100

# rho 		= 0.95
rho 		= 0.8

randomE		= rnorm(N, 0, 1)

# Random Walk
RandomWalk	= cumsum(randomE) + StartVal

# AR 1 - mean reverted
ar1 	= rep(0, N + 1)
ar1[1] 	= StartVal

for (t in 2:(N + 1)) {
	ar1[t] = rho * ar1[t - 1] + StartVal * (1 - rho) + randomE[t-1]
}

ar1 = ar1[-1]

# Plot Random Walk + AR 1
plot(1:N, RandomWalk, type = "o", col = "blue3", lwd = 2, ylab = "X", xlab = "Time Period",
	main = "Random Walk vs. AR(1)", pch=15, , ylim=c(80,105))
lines(1:N, ar1, type = "o", col = "red", pch=15)
