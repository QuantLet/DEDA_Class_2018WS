graphics.off()
rm(list = ls(all = TRUE))
#setwd("C:/...")

x1 = read.table("Bay9906_standLogRet.dat")
x2 = read.table("BMW9906_standLogRet.dat")
x3 = read.table("Sie9906_standLogRet.dat")
x4 = read.table("Vow9906_standLogRet.dat")

plot(x1[, 1], ylim = c(-6, 8), main = "Daily Standardized Log-Returns of Bayer",
    type = "l", col = "blue", xlab = "", ylab = "")
plot(x2[, 1], ylim = c(-6, 8), main = "Daily Standardized Log-Returns of BMW",
    type = "l", col = "blue", xlab = "", ylab = "")
plot(x3[, 1], ylim = c(-6, 8), main = "Daily Log-Returns of Siemens",
    type = "l", col = "blue", xlab = "", ylab = "")
plot(x4[, 1], ylim = c(-6, 8), main = "Daily Standardized Log-Returns of Volkswagen",
    type = "l", col = "blue", xlab = "", ylab = "")
