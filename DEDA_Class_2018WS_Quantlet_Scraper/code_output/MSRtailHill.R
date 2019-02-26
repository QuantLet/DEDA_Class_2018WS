rm(list=ls(all=TRUE))
#setwd("C:/...")

a  = read.table('Bay9906_close_2kPoints.txt')
b  = read.table('Bmw9906_close_2kPoints.txt')
c  = read.table('Sie9906_close_2kPoints.txt')
d  = a + b + c

n1 = dim(d)[1]
x  = log(d[1:n1-1, ]/d[2:n1,]) #negative log-returns
n  = length(x)
x  = sort(x, decreasing = TRUE)
k  = 100
x  = x[1:k]

gammaH = mean(log(x))-log(x[k])
alphaH = (mean(log(x)) -log(x[k]))^(-1)
sigmaH = x[k]*(k/n)^(1/alphaH)
sigmaH
