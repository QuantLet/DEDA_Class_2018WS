rm(list = ls(all = TRUE))
#setwd("C:/...")

#install.packages("lattice")
library(lattice)

u = seq(0,1,by=0.02)
v = u
m = length(u) 
n = length(v)
U = matrix(rep(u, each = n), nrow = n)
V = matrix(rep(v, m), nrow = n)

p = 2
gcopuly = (U^(-p) + V^(-p)-1)^(-2-1/p)*(U*V)^(-p-1)*(p+1)

s = expand.grid(u = u, v = v)
wireframe(gcopuly ~ u*v, s, shade = TRUE, xlab = "X", ylab = "Y",
    zlab = "Z", main = "Clayton Copula Density, theta = 2",
    scales = list(arrows = FALSE))
