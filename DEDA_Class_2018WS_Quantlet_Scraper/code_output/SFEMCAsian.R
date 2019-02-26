# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# settings
s.0   = 35     # current stock price
k     = 40     # strike price
tau   = 1      # time to maturity in years
n.t   = 88     # number of time steps (used for stock simulation and averaging)
r     = 0.03   # annual interest rate
sigma = 0.2    # volatility
n.mc  = 10^5   # number MC samples

# function for simulating stock price by geometric brownian motion
MCPath = function(s.0, tau, n.t, r, sigma) {
    dt  = tau/n.t
    t   = seq(0, tau, by = dt)
    s.t = s.0 * exp((r - sigma^2/2) * t + sigma * sqrt(dt) * c(0, cumsum(rnorm(n.t))))
    return(s.t)
}

# function for calculating MC estimate and confidence intervall of asian option price
MCAsian = function(s.0, k, tau, n.t, r, sigma, n.mc) {
    
    v.sum         = 0
    v.squared.sum = 0
    
    for (i in 1:n.mc) {
        
        #cat("\r progress: ", round(i/n.mc*100), "% ")  # progress information
        
        s.t = MCPath(s.0, tau, n.t, r, sigma)
        
        # geometric and arithmetic average
        s.avg.geom = exp(sum(log(s.t))/length(s.t))
        s.avg.arit = sum(s.t)/length(s.t)
        s.avg      = c(s.avg.arit, s.avg.geom)
        
        # average strike payoff
        v.asc = pmax(0, s.t[length(s.t)] - s.avg)  # call
        v.asp = pmax(0, s.avg - s.t[length(s.t)])  # put
        
        # average price payoff
        v.apc = pmax(0, s.avg - k)  # call
        v.app = pmax(0, k - s.avg)  # put
        
        # discounted payoff
        v = exp(-r * tau) * c(v.asc, v.asp, v.apc, v.app)
        
        # calculate sums for MC estimate and confidence intervall
        v.sum         = v.sum + v
        v.squared.sum = v.squared.sum + v^2
    }
    
    # MC estimate
    v.mc = v.sum/n.mc
    
    # 95% confidence intervall
    conf95 = 1.96 * 1/sqrt(n.mc) * sqrt(1/(n.mc - 1) * 
            (v.squared.sum - 2 * v.mc * v.sum + n.mc * v.mc^2))
    
    return(rbind(v.mc, conf95))
}

# main
value = MCAsian(s.0, k, tau, n.t, r, sigma, n.mc)

# plot results
options(digits = 3)
cat(" -------------------------------------------------", "\n", 
    "Price of Asian options and 95% CI by Monte Carlo ",  "\n", 
    "-------------------------------------------------",  "\n",
    "s.0   = ", sprintf("%.2f", s.0),   "\n", 
    "k     = ", sprintf("%.2f", k),     "\n", 
    "tau   = ", sprintf("%.2f", tau),   "\n", 
    "n.t   = ", sprintf("%.0f", n.t),   "\n", 
    "r     = ", sprintf("%.2f", r),     "\n", 
    "sigma = ", sprintf("%.2f", sigma), "\n", 
    "n.mc  = ", sprintf("%.0f", n.mc),  "\n", 
    "-------------------------------------------------", "\n", 
    "avg. strike|  arithm. avg.         geom. avg.",     "\n", 
    "-----------|-------------------------------------", "\n", 
    "      call |", sprintf("%.3f", value[1, 1]), "+/-", sprintf("%.3f", value[2, 1]), "   ", 
                    sprintf("%.3f", value[1, 2]), "+/-", sprintf("%.3f", value[2, 2]), "\n", 
    "      put  |", sprintf("%.3f", value[1, 3]), "+/-", sprintf("%.3f", value[2, 3]), "   ", 
                    sprintf("%.3f", value[1, 4]), "+/-", sprintf("%.3f", value[2, 4]), "\n", 
    "-----------|-------------------------------------", "\n", 
    " avg. price|  arithm. avg.         geom. avg.",     "\n", 
    "-----------|-------------------------------------", "\n", 
    "      call |", sprintf("%.3f", value[1, 5]), "+/-", sprintf("%.3f", value[2, 5]), "   ", 
                    sprintf("%.3f", value[1, 6]), "+/-", sprintf("%.3f", value[2, 6]), "\n", 
    "      put  |", sprintf("%.3f", value[1, 7]), "+/-", sprintf("%.3f", value[2, 7]), "   ", 
                    sprintf("%.3f", value[1, 8]), "+/-", sprintf("%.3f", value[2, 8]), "\n")
