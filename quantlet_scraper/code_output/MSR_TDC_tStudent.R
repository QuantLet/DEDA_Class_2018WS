rm(list = ls(all = TRUE))
# setwd('C:/...')

install.packages("lattice")
#library(lattice)

d = 3:103
r = seq(-1, 1, by = 0.02)
m = length(d)
n = length(r)
D = matrix(rep(d, each = n), nrow = n)
R = matrix(rep(r, m), nrow = n)

x = t(R)
y = t(D)
N = length(x)

t = 2 - 2 * pt(sqrt(y + 1) * sqrt(1 - x)/sqrt(1 + x), y + 1)

s = expand.grid(d = d, r = r) 

wireframe(t ~ d*d, s, shade = TRUE, xlab = "Degrees of Freedom",
    ylab = "Correlation", zlab = "Tail Dep Coef", 
    main = "Tail Dependence Coefficient for t-Student Copula", 
    scales = list(arrows = FALSE))
