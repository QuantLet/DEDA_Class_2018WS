graphics.off()
rm(list = ls(all = TRUE))
#setwd("C:/...")

x1 = read.table("BMW9906_standLogRet.dat")
x2 = read.table("Vow9906_standLogRet.dat") 

plot(x1$V1, x2$V1, col = "blue", xlab = "Standardized Log-Returns of BMW",
    ylab = "Standardized Log-Returns of Volkswagen",
    xlim = c(-6.2, 7), ylim = c(-5.5, 5.5), pch = 20)
