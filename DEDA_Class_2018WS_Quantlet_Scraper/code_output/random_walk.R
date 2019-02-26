N			= 50
StartVal	= 100

randomE	= rnorm(N, 0, 1)

# Random Walk
RandomWalk = cumsum(randomE) + StartVal

# Plotting two types
par(mfrow=c(1,2))

plot(1:N, RandomWalk, type = "o", col = "blue3", lwd = 2, ylab = "X", xlab = "Time Period", main = "Random Walk - lines/points", pch=15)
plot(1:N, RandomWalk, type = "s", col = "blue3", lwd = 2, ylab = "X", xlab = "Time Period", main = "Random Walk - steps")
