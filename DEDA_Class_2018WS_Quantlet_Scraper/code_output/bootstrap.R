# clear history
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("MASS")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


data(mammals)

x            = mammals$body
y            = mammals$brain
n            = length(x)                      #set n to the number of observations
n.sim        = 1e4                            #set number of bootstrap samples
rho.boot     = numeric(n.sim)                 #create a vector to hold result

for (h in 1:n.sim) {
    s        = sample(n, replace=TRUE)        #draw a sample with replacement from 1:n
    cur.x    = x[s]                           #use it to subset x and y --- ''bootstrap sample''
    cur.y    = y[s]
    rho.boot = cor(cur.x, cur.y)              #compute the corelation in the bootstrap sample
}

alpha        = 0.95
quantile(rho.boot, c(alpha/2, 1-alpha/2))     #Return empirical quantiles of the bootstrapped correlation coefficient
