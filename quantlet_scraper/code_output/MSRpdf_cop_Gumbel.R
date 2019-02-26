rm(list = ls(all = TRUE))
#setwd("C:/...")

#install.packages("lattice")
library(lattice)

u = seq(0, 1, by = 0.02)
v = u
m = length(u) 
n = length(v)
U = matrix(rep(u, each = n), nrow = n)
V = matrix(rep(v, m), nrow = n)

p   = 2
arg = (-log(U))^p + (-log(V))^p

gcopuly = exp(-arg^(1/p))/U/V * (log(U)*log(V))^(p-1) * arg^(1/p-2)*(p-1+arg^(1/p))

s = expand.grid(u = u, v = v)
wireframe(gcopuly ~ u*v, s, shade = TRUE, xlab = "X", ylab = "Y", zlab = "Z",
    main = "Gumbel Copula Density, \\theta = 2",scales = list(arrows = FALSE))
