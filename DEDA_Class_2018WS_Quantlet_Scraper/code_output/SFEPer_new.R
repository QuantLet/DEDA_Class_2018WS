# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

soi = scan("http://www.stat.pitt.edu/stoffer/tsa2/data/soi.dat")

# Time domain
par(mfrow = c(1, 1))
plot(soi, type = "l", col = "blue3")

par(mfrow = c(2, 1))
acf(soi, xlab = "Lag Time", lwd = 4, main = "")
pacf(soi, xlab = "Lag Time", ylab = "PACF", lwd = 4, main = "")

# Frequency domain
par(mfrow = c(1, 1))
soi.per = spec.pgram(soi, taper = 0, log = "no", col = "red3", lwd = 4, main = "", 
    sub = "", ylab = "Spectrum", xlab = "Frequency")
abline(v = 1/12, lty = "dotted")
abline(v = 1/48, lty = "dotted")
