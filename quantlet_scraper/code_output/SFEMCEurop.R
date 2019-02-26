# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# settings
s.0   = 120   # current stock price
k     = 130   # strike price
tau   = 0.6   # time to maturity in years
r     = 0.03  # annual interest rate
sigma = 0.2   # volatility
n.mc  = 10^5  # number MC samples

# function for simulating stock price by geometric brownian motion
MCPath = function(s.0, tau, n.t, r, sigma) {
    dt  = tau/n.t
    t   = seq(0, tau, by = dt)
    s.t = s.0 * exp((r - sigma^2/2) * t + sigma * sqrt(dt) * c(0, cumsum(rnorm(n.t))))
    return(s.t)
}

# function for calculating MC estimate and confidence intervall of european option price
MCEurop = function(s.0, k, tau, n.t, r, sigma, n.mc) {
    v.sum         = 0
    v.squared.sum = 0

    for (i in 1:n.mc) {
        s.t = MCPath(s.0, tau, n.t = 1, r, sigma)
        c   = pmax(0, s.t[length(s.t)] - k)  # call payoff
        p   = pmax(0, k - s.t[length(s.t)])  # put payoff
        v   = exp(-r * tau) * c(c, p)   
    
        # calculate sums for MC estimate and confidence intervall
        v.sum         = v.sum + v
        v.squared.sum = v.squared.sum + v^2
    }

    # MC estimate
    v.mc = v.sum/n.mc
    
    # 95% confidence intervall
    conf95 = 1.96 * 1/sqrt(n.mc) * sqrt(1/(n.mc - 1) * (v.squared.sum - 2 * v.mc * v.sum + n.mc * v.mc^2))
    
    return(rbind(v.mc, conf95))
}

# function for calculating the Black-Scholes price of European option
BSEurop = function(s0, k, tau, r, sigma) {
    d1 = (log(s0/k) + (r + (sigma^2/2)) * tau)/(sigma * sqrt(tau))
    d2 = d1 - (sigma * sqrt(tau))
    c  = s0 * pnorm(d1) - k * exp(-r * tau) * pnorm(d2)     # call
    p  = -s0 * pnorm(-d1) + k * exp(-r * tau) * pnorm(-d2)  # put
    return(c(c, p))
}

# main
mc = MCEurop(s.0, k, tau, n.t = 1, r, sigma, n.mc)
bs = BSEurop(s.0, k, tau, r, sigma)

# plot results
options(digits = 3)
cat(" ------------------------------------------------", "\n", 
    "s.0   = ", sprintf("%.2f", s.0),   "\n", 
    "k     = ", sprintf("%.2f", k),     "\n", 
    "tau   = ", sprintf("%.2f", tau),   "\n", 
    "r     = ", sprintf("%.2f", r),     "\n", 
    "sigma = ", sprintf("%.2f", sigma), "\n", 
    "n.mc  = ", sprintf("%.0f", n.mc),  "\n", 
    "------------------------------------------------", "\n", 
    "Monte Carlo price of European options and 95% CI", "\n",     
    "------------------------------------------------", "\n",     
    "call |", sprintf("%.3f", mc[1, 1]), "+/-", 
              sprintf("%.3f", mc[2, 1]), "\n", 
    "put  |", sprintf("%.3f", mc[1, 2]), "+/-", 
              sprintf("%.3f", mc[2, 2]), "\n", 
    "------------------------------------------------", "\n", 
    "Black-Scholes price of European options", "\n", 
    "------------------------------------------------", "\n", 
    "call |", sprintf("%.3f", bs[1]), "\n", 
    "put  |", sprintf("%.3f", bs[2]), "\n")
