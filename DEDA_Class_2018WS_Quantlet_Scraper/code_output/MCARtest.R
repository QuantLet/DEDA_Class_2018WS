# clear all variables
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
# libraries = c("Rlab", "fda", "stats", "MASS", "splines", "psych", "tensor", "crs", "orthogonalsplinebasis", "EQL")
libraries = c("Rlab", "EQL")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


######### Generate Data ######### 

m       = 20  #alternative choices possbile
n       = 500  #sample size
c.eig   = m
s       = -1.5  #alternative: -2, -2.5
w.const = 0.3  #=cor(W,Y)

set.seed(123456)

unobs = rnorm(3 * n)
W     = unobs[1:n]  # instrument
Y     = w.const * unobs[1:n] + sqrt(1 - w.const^2) * unobs[(n + 1):(2 * n)]  #dependent variable
U     = unobs[(2 * n + 1):(3 * n)]

delta      = c()  #missing values indicator
delta[1:n] = 1
ones       = rep(1, 1, n)
YX.vec     = 0.3 * Y + sqrt(1 - 0.3^2) * U
q.const    = quantile(YX.vec, probs = 0.2)

f.fct      = function(x) {
    as.numeric(x &gt;= q.const) + 0.1 * as.numeric(x &lt; q.const)
}

my = f.fct(YX.vec)
for (i in 1:n) {
    delta[i] &lt;- rbern(1, my[i])
}

Y.mis = delta * Y

#########  Alternative (Own Data Set) Data Input ########## setwd('') data&lt;- dataset&lt;-data.frame(data) attach(dataset)

################# Variable Input m=10 # adjustable: input number of orthogonalized Hermite polynomials s=c(-1.5) # alternative 2, 2.5 n &lt;- #sample size c.eig &lt;-m Y&lt;- #dependent variable W&lt;-#instrument
################# w.const=cor(W,Y) delta &lt;- #set equal to a (nx1) indicator vector, where delta=1, if responce is available and delta=0, otherwise Y.mis&lt;-Y*delta


######### MCAR TEST ######### 

test = matrix(nrow = 1, ncol = 2)

######### Test Statistic ######### 
Weights = vector()
for (j in 1:m) Weights[j] = j^s

BasWf.mat = mat.or.vec(n, m)
for (i in 1:m) {
    BasWf.mat[, i] = hermite(W, i)/sqrt(factorial(i))
}


# Alternative: Transform W to unit interval [0,1] and use cosine basis function cos.F=function(x,j){sqrt(2)*cos((j)*pi*x)} BasWf.mat=mat.or.vec(n,m) for(i in 1:m){BasWf.mat[,i] =
# cos.F((W-min(W))/(max(W)-min(W)),i)} BasWf.mat = gsl.bs(W,degree=17, nbreak=6) knots=expand.knots(c(0,as.vector(attr(bs(W, df=17, degree=4), 'knots')),1)) BasWf.mat=evaluate(OBasis(knots), W)

h.hat     = sum(delta)/n
coef0.vec = t(delta - h.hat) %*% BasWf.mat/n
Test0     = n * sum((Weights * coef0.vec) * (Weights * coef0.vec))

#########  Critical Values ######### 
C.mat     = matrix(rnorm(c.eig * 10000), c.eig, 10000)^2
Wf.mat    = BasWf.mat %*% diag(Weights)
eps.1.mat = diag(as.vector(delta - h.hat)) %*% Wf.mat
eps.h.mat = as.vector(delta - h.hat) %*% (delta %*% Wf.mat/n)
eps.mat   = eps.1.mat - eps.h.mat
Sigma.mat = t(eps.mat) %*% eps.mat/n
eig.vec   = rev(sort(eigen(Sigma.mat, symmetric = TRUE)$values))
CC        = sort(eig.vec[1:c.eig] %*% C.mat)

#########  Test Result ######### 
test[1, 1] = as.numeric(Test0)  #Value of Test Statistic
test[1, 2] = as.numeric(Test0 &gt; CC[9500])  #Critical Value
test
