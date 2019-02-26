# clear variables and close windows
  rm(list = ls(all = TRUE))
  graphics.off()

# install and load packages
  libraries = c("tseries", "fGarch", "rugarch")
  lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
  })
  lapply(libraries, library, quietly = TRUE, character.only = TRUE)
  
#Monte Carlo Study
# k: Number of repetitions
# n: Number of observations

k = 1000
n = c(100, 250, 500, 1000)

#model parameters: 
alpha0 = 0.9
omega0 = 0.2

#Simulation:
for (i in 1:length(n)){
  alpha = rep(0, k)
    for (l in 1:k){
      #Simulate ARCH(1) time series  
      ts_sim = garchSim(spec = garchSpec(model = list(alpha = alpha0, beta = 0, omega = omega0)), n = n[i]) 
      
      #fit model to QMLE-method      
      fit = try(garchFit(~ garch(1,0), data = ts_sim, cond.dist = "QMLE", trace = FALSE))
        if (l &lt; k){
          alpha[l] = fit@fit$par[3]} 
            else{alpha[l] =  fit@fit$par[3]
                  assign(paste0("alpha", n[i]), alpha)
            }
        }
}


#Avg. deviation from true parameter
dev = function(x, alpha0){ 
          x = as.vector(x)
          y = sqrt(sum((x - alpha0)^2)*k^-1)
      return(y)
    }
  
# Percentage of alpha estimates &gt;= 1
perc = function(x){
        x = as.vector(x)
        x = round(x, 4)
        y = sum(x == 1 | x &gt; 1)
    return(y/length(x))
}  
  
  
#Merge results:  
n.obs     = rbind(n[1], n[2], n[3], n[4]) 
average   = rbind(mean(alpha100), mean(alpha250), mean(alpha500), mean(alpha1000))
deviation = rbind(dev(alpha100, alpha0),dev(alpha250, alpha0),dev(alpha500, alpha0),dev(alpha1000, alpha0))
leq.one   = rbind(perc(alpha100), perc(alpha250), perc(alpha500), perc(alpha1000))
  
#Create Table &amp; save output
  
Results = cbind(n.obs, average, deviation, leq.one)
Results = data.frame(round(Results, 3))  
names(Results) = c("n.obs", "average", "deviation", "leq 1")
write.table(Results, file = "MC_Summary.csv", sep = ",", dec = ".", row.names = FALSE)
write.table(cbind(alpha100, alpha250, alpha500, alpha1000), file = "MC_parameters.csv", sep = ",", dec = ".", row.names = FALSE)
