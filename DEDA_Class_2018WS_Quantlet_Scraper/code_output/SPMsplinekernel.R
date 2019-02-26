# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

u = seq(-10, length = 200, by = 0.1)  # creates the variable x
k = 0.5 * (exp(-abs(u)/sqrt(2))) * (sin(abs(u)/sqrt(2) + pi/4))  # computes the approximative spline kernel

plot(u, k, type = "n", ylab = "density", xlab = "u", xlim = c(-10, 10), ylim = c(-0.02, 
    0.4))
title("The effective spline kernel")
lines(u, k, col = "blue3", lwd = 3)

# y = 0.5* (exp(-abs(u)/sqrt(2)))* (sin(abs(u)/sqrt(2)+pi/4))

# load data
data = read.table("agg73sh.dat")
x = data[, 1]
y = data[, 4]
h = 0.25
grid = seq(min(x), max(x), length = length(x))
mh = sker(x = grid, vecX = x, vecY = y, h = h, K = 5)  # Nadaraya Watson estimator for effective Spline kernel

dev.new()
plot(x, y, pch = 20, col = "skyblue", xlab = "Net-income", ylab = "Food", cex = 0.7, 
    sub = paste("Bandwidth, h = ", h))
title("Nadaraya-Watson Estimate employing effective spline kernel")
lines(x, GMspline, lwd = 2)
