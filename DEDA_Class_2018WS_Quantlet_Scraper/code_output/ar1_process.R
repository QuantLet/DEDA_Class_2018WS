N			= 50
StartVal	= 100

rho = 0.8

randomE	= rnorm(N, 0, 1)

# AR 1 - mean reverted
ar1 = rep(0, N + 1)
# first element is the starting value
ar1[1] = StartVal

for (t in 2:(N + 1)) {
	ar1[t] = rho * ar1[t - 1] + StartVal * (1 - rho) + randomE[t-1]
}

# discard the starting value and keep X1,...,X_N
ar1 = ar1[-1]

# Plotting two types
par(mfrow=c(1,2))

plot(1:N, ar1, type = "o", col = "blue3", lwd = 2, ylab = "X", xlab = "Time Period", main = "AR(1) - lines/points", pch=15)
plot(1:N, ar1, type = "s", col = "blue3", lwd = 2, ylab = "X", xlab = "Time Period", main = "AR(1) - steps")
