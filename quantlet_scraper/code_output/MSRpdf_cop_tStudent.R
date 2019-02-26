rm(list=ls(all=TRUE))
#setwd("C:/...")

#install.packages("lattice")
library(lattice)

v    = 3
r    = 0.4
wyzn = 1 - (r^2)
u    = seq(0, 1, by = 0.01)
w    = u
d    = length(u) 
n    = length(w)
U    = matrix(rep(u, each = n), nrow = n)
W    = matrix(rep(w,d), nrow = n)

y1   = qt(U, v)
y2   = qt(W, v)

y    = cbind(as.vector(y1), as.vector(y2))
R    = cbind(c(1, r), c(r, 1))

g    = (wyzn)^(-0.5)*gamma(v/2+1)*gamma(v/2)/gamma((v+1)/2)^2

d2   = d^2
c2   = (1:d^2)

for (i in 1:d^2){
	c2[i]=((1 + y[i,1]^2/v)*(1 + y[i,2]^2/v))^((v+1)/2)/
    (1 + (y[i, ]%*%solve(R)%*%y[i, ])/v)^(v/2 + 1)
	}
	
c = c2*g
f = matrix(c, nrow = d, byrow = T)
z = cbind(as.vector(U), as.vector(W), c)

s = expand.grid(u = u, w = w)
wireframe(f ~ u*w, s, shade = TRUE, xlab = "X", ylab = "Y", zlab = "Z", 
    main = "t-Student Copula Density, \\nu = 3, r = 0.4", scales = list(arrows = FALSE))
