rm(list = ls(all = TRUE))
#setwd("C:/...")

x1 = read.table("BMW9906_standLogRet.dat")
x2 = read.table("Vow9906_standLogRet.dat")

x1 = pt(x1[,1], 8)
x2 = pt(x2[,1], 7)

x  = c(0.9, 0.9, 1.1, 1.1)
y  = c(0.9, 1.1, 1.1, 0.9)

plot(x1, x2, pch = 20, col = "blue", xlab = "BMW", ylab = "Volkswagen")
abline(h = 0.9, v = 0.9, col = "red", lwd = 3)
polygon(x, y, col = "yellow", density = 10)
