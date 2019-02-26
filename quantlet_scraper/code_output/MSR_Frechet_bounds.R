rm(list=ls(all=TRUE))
#setwd("C:/...")

#install.packages("lattice")
library(lattice)

x  = seq(0, 1, by = 0.01)
y  = x
m  = length(x)
n  = length(y)
X  = matrix(rep(x, each = n), nrow = n)
Y  = matrix(rep(y, m), nrow = n)
m  = pmin(X, Y)
w  = pmax(X + Y - 1, 0)
pi = X * Y

s = expand.grid(x = x, y = y)
wireframe(m ~ x * y, s, shade = TRUE, 
          xlab = "X", ylab = "Y", 
          zlab = "Z", main = "M(x,y)", 
          scales = list(arrows = FALSE))

s = expand.grid(x = x, y = y)
wireframe(w ~ x * y, s, shade = TRUE, 
          xlab = "X", ylab = "Y", zlab = "Z", 
          main = "W(x, y)", scales = list(arrows = FALSE))

s = expand.grid(x = x, y = y) 
wireframe(pi ~ x*y, s, shade = TRUE, xlab = "X", ylab = "Y", zlab = "Z",
    main = "Pi(x, y)", scales = list(arrows = FALSE))
