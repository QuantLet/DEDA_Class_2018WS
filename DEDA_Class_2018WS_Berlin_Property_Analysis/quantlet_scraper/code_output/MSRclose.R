rm(list=ls(all=TRUE))
#setwd("C:/...")

x1 = read.table('BAYER_close_99_06.dat')
x2 = read.table('BMW_close_99_06.dat')
x3 = read.table('SIEMENS_close_99_06.dat')
x4 = read.table('VOLKSWAGEN_close_99_06.dat')

x1 = as.matrix(x1)
x2 = as.matrix(x2)
x3 = as.matrix(x3)
x4 = as.matrix(x4)

t = seq(23,dim(x1)[1], by = 261)

plot(x3[,2],type = "l", col = "blue", ylim = c(0, 140), 
    xlab = "", ylab = "", main = 'Closing Prices for German Companies', xaxt = "n")
lines(x1[, 2], lty = 2)
lines(x2[, 2], col = "red", lty = 3)
lines(x4[, 2], col = "green", lty = 4)
axis(1, at = t, labels = c(1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006))
