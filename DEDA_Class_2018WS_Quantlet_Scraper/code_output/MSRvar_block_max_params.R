rm(list = ls(all = TRUE))
#setwd("C:/...")

k  =  t(read.table("kappa_bMax_Portf.txt"))
a  =  t(read.table("alpha_bMax_Portf.txt"))
b  =  t(read.table("beta_bMax_Portf.txt"))

plot(k, type = "l", col = "blue", ylim = c(-1, 6), ylab = "", xlab = "", main = "Parameters in Block Maxima Model", axes = FALSE)
lines(a, col = "red")
lines(b, col = "magenta")
box()
axis(1, c(261, 521, 782, 1043, 1304, 1566, 1826)-250, c("Jan 2000", "Jan 2001", "Jan 2002", "Jan 2003", "Jan 2004", "Jan 2005", "Jan 2006"))
axis(2)
legend("topright", c("Shape Parameter", "Scale Parameter", "Location Parameter"), pch = c(15, 15, 15), col = c("blue", "red", "magenta"))
