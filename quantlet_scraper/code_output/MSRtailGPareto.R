rm(list = ls(all = TRUE))
#setwd("C:/....")

#install.packages("QRM")
library(QRM)

a    = read.table('Bay9906_close_2kPoints.txt')
b    = read.table('Bmw9906_close_2kPoints.txt')
c    = read.table('Sie9906_close_2kPoints.txt')
d    = a + b + c

n1   = dim(d)[1]
x    = log(d[1:n1-1, ]/d[2:n1, ]) #negative log-returns
n    = 100                     

gpd  = fit.GPD(x, nextremes = n, information = "observed") 
t    = (1:n)/(n+1)
y1   = qGPD(t, gpd$par.ests[1], gpd$par.ests[2])                   
 
gpdt = sort(gpd$data) - gpd$threshold                             
y2   = pGPD(gpdt, gpd$par.ests[1], gpd$par.ests[2])            
                                                    
plot(gpdt, y1, col = "blue", pch = 21, bg = "blue", ylim = c(0, 0.06), xlab = "",
    ylab = "", main = "QQ plot, Generalized Pareto Distribution")
lines(y1, y1, type = "l", col = "red", lwd = 2)  

plot(y2, t, col = "blue", pch = 21, bg = "blue", xlab = "", ylab = "", 
    main = "PP plot, Generalized Pareto Distribution")
lines(y2, y2, type = "l", col = "red", lwd = 2)
