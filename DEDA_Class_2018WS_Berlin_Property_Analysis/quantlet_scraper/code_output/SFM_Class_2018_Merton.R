## Clearing Variables and Close Windows
rm(list = ls(all = TRUE))
graphics.off()

## Loading Libraries
libraries = c("ggplot2", "reshape2", "stats", "zoo", "tidyr", "lattice")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# fix values
set.seed(1)

# Poisson generator
PPgen             =  function(lambda){
X                 = 0
Sum               = 0
flag              = 0
while (flag==0){
E                 = -log(runif(1))
Sum               = Sum + E
    if(Sum &lt; lambda){ 
    X             = X+1
    } 
  else { 
  flag            = 1
  }
  
  }
  return(X)
  
}

# Merton model

Merton            = function(mu,sigma,lambda,mu_y,sigma_y, N, T) {
t                 = seq(0,T)/N
X                 = rep(0, N+1)
X[1]              = 0
F                 = rep(0, N+1)
I                 = rep(0, N)
h                 = T/N
 
for(i in 1:N){
I[i]              = PPgen(h*lambda)
if(I[i ]== 0){
F[i]              = 0
    } 
  else {
F[i]              = mu_y*I[i]+ sqrt(sigma_y)*sqrt(I[i])*rnorm(1)
  }
X[i+1]            = X[i] + mu*h+sigma*sqrt(h)*rnorm(1)+F[i]
    }
  return(X)
}

## Generate 10 Poisson Process Paths and 10 Compound Poisson Process Paths
lambda            = 10
mu                = 2
sigma             = 3
N                 = 1000
T                 = 1
stim_up           = 10
mu_y              = 2.5
sigma_y           = 2

col_name = c('S1', 'S2','S3', 'S4','S5', 'S6','S7', 'S8','S9', 'S10', 'time')

## Initialization
S_Merton           = matrix(0, stim_up, N+1)

## Generating 
for(i in 1:stim_up){
S_Merton[i,]       = Merton(mu,sigma,lambda,mu_y,sigma_y, N, T)
  
  }
S_Merton = data.frame(t(S_Merton)) 

S_Merton$time      = c(0:1000)
colnames(S_Merton) = col_name 


# Plot price paths
pic_1              = xyplot(S1 + S2 + S3 + S4 + S5 + S6 + S7 + S8 + S9 + S10 ~ time, data = S_Merton, 
             type  = "l", 
             xlab  = list(label = "Time", cex = 1), 
              ylab = list(label = "Simulated asset price", cex =1), 
            scales = list(tck=c(1,0), x=list(cex=1), y=list(cex=1)),
          auto.key = FALSE)


print(pic_1)
