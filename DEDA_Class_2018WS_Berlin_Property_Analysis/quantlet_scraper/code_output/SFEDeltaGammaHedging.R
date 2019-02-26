rm(list = ls(all = TRUE))
graphics.off()

#S      = c(60, 58, 58.5, 59, 59.5, 60, 60.5, 61, 61.5, 62)      # Initial price
S      = c(60, 50, 55, 60, 65, 70)      # Initial price
K1     = 60      # strike price
K2     = 65      # strike price
r      = 0.08    # current expectation of returns
sigma  = 0.3     # volatility , std deviations around the drift
T1     = 90/365  # time to maturity is 90 days
T2     = 60/365  # time to maturity is 60 days
h0     = 1000    # number of Call1 options short
dt     = 1/365   # time step

# Gamma of BS call option
GammaCall = function(S, K, r, sigma, T, t){
    d1 = (log(S/K) + (r + sigma^2/2)*(T - t))/(sigma*sqrt(T - t))
    gamma  = dnorm(d1)/(S*sigma*sqrt(T - t))
    return(gamma)
}
# Delta of BS call option
DeltaCall = function(S, K, r, sigma, T, t){
  d1    = (log(S/K) + (r + (sigma^2)/2)*(T - t))/(sigma*sqrt(T - t));
  delta = pnorm(d1)
  return(delta)
}
# BS call option
BSCallPrice = function(S, K, r, sigma, T, t){
    d1    = (log(S/K) + (r + sigma^2/2)*(T - t))/(sigma*sqrt(T - t))
    d2    = d1 - sigma*sqrt(T - t)
	price = S*pnorm(d1) - K*exp(-r*(T - t))*pnorm(d2)
	return(price)
}

# initialize vectors
Call1     = c() 
Delta1    = c() 
Gamma1    = c() 
Call2     = c() 
Delta2    = c() 
Gamma2    = c()
h2        = c() # number of options
h1        = c() # number of shares
money     = c() # balance
PV        = c() # portfolio value
PV.tmp    = c()
cost.share= c()
cost.Call2= c()
diff.h1   =c()
diff.h2   =c()
diff.money=c()
cum.cost  = c()
delta.v   = 0
gamma.v   = 0
delta.n   = 0
gamma.n   = 0
Portfolio = matrix(0, length(S), 11)
t         = c(0, dt*(1:length(S)))

for(i in 1:length(S)){

Call1[i]  = BSCallPrice(S[i], K1, r, sigma, T1, t[i])
Call1[i]    # Call price option 1  
Delta1[i] = DeltaCall(S[i], K1, r, sigma, T1, t[i])
Delta1[i]   # Delta of option 1
Gamma1[i] = GammaCall(S[i], K1, r, sigma, T1, t[i])
Gamma1[i]   # Gamma of option 1

Call2[i]  = BSCallPrice(S[i], K2, r, sigma, T2, t[i])
Call2[i]    # Call price of option 2
Delta2[i] = DeltaCall(S[i], K2, r, sigma, T2, t[i])
Delta2[i]   # Delta of option 1
Gamma2[i] = GammaCall(S[i], K2, r, sigma, T2, t[i])
Gamma2[i]   # Gamma of option 1


if(i == 1){
    h2[i] = h0*Gamma1[i]/Gamma2[i]       # number of options
    h1[i] = Delta1[i]*h0-h2[i]*Delta2[i] # number of stocks
    cost.share[i] = S[i]*h1[i]           # costs of shares
    cost.Call2[i] = Call2[i]*h2[i]       # costs of call option 2
    cum.cost[i]   = c(cost.share[i]+cost.Call2[i]) # costs
    money[i]      = h0*Call1[i] - h1[i]*S[i] - h2[i]*Call2[i] # bank account
    PV[i]         = h1[i]*S[i] + money[i] -h0*Call1[i] + h2[i]*Call2[i] # portfolio value
}else if(i&gt;1){ # for t &gt; 0
    PV.tmp[i]     = -h0*Call1[i] + h1[i-1]*S[i] + h2[i-1]*Call2[i] -     money[i-1]*exp(r*t[i]) # new portfolio value 
    delta.v[i] = h1[i-1] -h0*Delta1[i] + h2[i-1]*Delta2[i] # Delta of portfolio
    gamma.v[i] = h2[i-1]*Gamma2[i] - h0*Gamma1[i] # Gamma of portfolio
    h2[i] = h0*Gamma1[i]/Gamma2[i]       # number of options
    h1[i] = Delta1[i]*h0-h2[i]*Delta2[i] # number of stocks
    delta.n[i] = h1[i] -h0*Delta1[i] + h2[i-1]*Delta2[i] # Delta after rebalancing
    gamma.n[i] = h2[i]*Gamma2[i] - h0*Gamma1[i] # Gamma after rebalancing
#    money[i] = h0*Call1[i] - h1[i]*S[i] - h2[i]*Call2[i] # bank account without compounding
    money_tmp = (money[length(money)]*exp(r*t[i]) + (h1[length(h1)-1]-h1[length(h1)])*S[i] + (h2[length(h2)-1] - h2[length(h2)])*Call2[i] )/exp(r*t[i]) # bank account with compounding
    money[i] = money_tmp 
    PV[i] = h1[i]*S[i] + money[i] -h0*Call1[i] + h2[i]*Call2[i] # Portfolio value
    diff.h1[i]    = h1[i]-h1[i-1]       # change in number of stocks
    diff.h2[i]    = h2[i]-h2[i-1]       # change in number of call options 2
    diff.money[i] = money[i]-money[i-1] # change in bank account
    cost.share[i] = S[i]*diff.h1[i]     # costs of stocks
    cost.Call2[i] = Call2[i]*diff.h2[i] # costs of calls 
    cum.cost[i]   = c(cum.cost[i-1]+cost.share[i]+cost.Call2[i]) # cumulative costs
}
    Portfolio[i,] = c(S[i], Call1[i], Call2[i], Delta1[i], Delta2[i], Gamma1[i], Gamma2[i], h0, h1[i], h2[i], money[i]) # portfolio
}
colnames(Portfolio) = c("S","C_1","C_2", "Delta_1","Delta_2","Gamma_1","Gamma_2","nOptionsShort","nOptionsLong","nStocks", "Balance")

Portfolio


# library(xtable)
# xtable((Portfolio[,1:7]),digits=3)
# xtable((Portfolio[,c(1,8:ncol(Portfolio))]),digits=3)
# write.table(Portfolio, "Portfolio.txt", sep="\t")
