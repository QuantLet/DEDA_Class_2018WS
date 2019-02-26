library(stats4)
library(quantmod)
library(sde)  ## Only for Simulation

rm(list = ls(all = TRUE))
graphics.off()

estimateCIR_OLS &lt;- function(y, dt = 1/256, correction = 0.004){
  
  value &lt;- y + correction
  diff &lt;- diff(value, 1)
  sq_r &lt;- sqrt(value)[-length(value)]
  
  # calculate variables for CIR OLS estimation
  y &lt;- diff/sq_r
  x1 &lt;- dt/sq_r
  x2 &lt;- sq_r*dt
  
  #estimate OLS without intercept
  OLS &lt;- lm(y~x1+x2 - 1)

  sigma &lt;- summary(OLS)$sigma                       # diffusion estimate
  a &lt;- OLS$coefficients[2]*-1                       # convergence speed estimate
  b &lt;- OLS$coefficients[1]/OLS$coefficients[2]*-1   # long time mean
  
  return(c(LTmean_b = b, convSpeed_a = a, sigma = sigma))
}

estimateCIR_ML &lt;- function(y, start_a, start_b, start_sigma, dt = 1/256, seed = 42, correction = 0.004){
  
  set.seed(seed) # set inital seed
  y &lt;- y + correction # correct ts 
  
  # define CIR function
  CIR &lt;- function(theta1, theta2, theta3) {
    n=length(y)
    dt=1/256
    return(-sum(dcCIR(x=y[2:n], Dt=dt, x0=y[1:(n-1)], theta=c(theta1, theta2, theta3), log=T)))
  }
  
  #estimate ML based on start values (here OLS)
  fit &lt;- mle(CIR, start=list(theta1=as.numeric(start_a * start_b), 
                             theta2=as.numeric(-start_a),
                             theta3=as.numeric(start_sigma)),
             method='L-BFGS-B',
             lower=c(0.0005,0.01,0.01),
             upper=c(1,Inf,1))
  
  result &lt;- c(LTmean_b = coef(fit)[1],  convSpeed_a = coef(fit)[2], sigma = coef(fit)[3])
  return(result)
}


#################
### execution ###
#################

# read data points
y &lt;- read.csv(file = "eonia_clean.csv")
#y &lt;- read.csv(file = "3Meuribor_clean.csv")

# time horizon
start &lt;- match("1999-01-04", y$Date)
end &lt;- match("2012-12-31", y$Date)
y &lt;- y[start:end, 2]

OLSestimates &lt;- estimateCIR_OLS(y, correction = 0)
MLestimates &lt;- estimateCIR_ML(y, 
                               start_a = OLSestimates[2], 
                               start_b = OLSestimates[1], 
                               start_sigma = OLSestimates[3], 
                               correction = 0)


##################
### simulation ###
##################

set.seed(42)
sde.sim(X0=y[1], theta=c(MLestimates[1], MLestimates[2], MLestimates[3]), model="CIR", N=256) -&gt; X1
plot(X1, main="Cox-Ingersoll-Ross")
