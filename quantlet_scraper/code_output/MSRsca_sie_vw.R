graphics.off()
rm(list=ls(all=TRUE))
#setwd("C:/...")

x1 = read.table("Sie9906_standLogRet.dat")
x2 = read.table("Vow9906_standLogRet.dat") 

plot(x1$V1, x2$V1, col = "blue", xlab = "Standardized Log-Returns of Siemens", 
    ylab = "Standardized Log-Returns of Volkswagen", 
    main = "Log-Returns of Siemens versus Volkswagen", 
    xlim = c(-6, 6), ylim = c(-6, 6), pch = 20)
