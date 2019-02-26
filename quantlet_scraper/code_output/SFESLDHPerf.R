# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Declare stock price variables
S0      = 49                                        # current stock price
sig     = 0.2                                       # volatility 

# Declare option pricing variables
r       = 0.05                                      # interest rate
K       = 50                                        # strike price
t0      = 0/52                                      # current time (1 week = 1/52)
mat     = 20/52                                     # maturity
steps   = 3000                                      # number of steps from t0 to maturity (30 steps per day)

# Black-Scholes option price
tau0    = mat - t0  # time to maturity
y       = (log(S0/K) + (r - (sig^2)/2) * tau0)/(sig * sqrt(tau0))
C       = S0 * pnorm(y + sig * sqrt(tau0)) - exp(-r * tau0) * K * pnorm(y)

# Declare path generating function
GeneratePaths = function(S0, sig, mat, K, r, t0, steps) {
    dt      = (mat - t0)/steps                      # period between steps
    t       = seq(t0, mat, dt)                      # maturity - t0 divided in n intervals
    tau     = mat - t                               # times to maturity
    
    # Simulate the stock price path
    Wt      = c(0, cumsum(sqrt(dt) * rnorm(steps)))
    S       = S0 * exp((r - 0.5 * sig^2) * t + sig * Wt)
    
    # Result
    result  = data.frame(Time = t, StockPrice = S)
    return(result)
}

# Declare hedge cost function
HedgeCosts = function(t, S, K, r, sig, mat, Dt, strategy) {
    # Dt is the period between observations (e.g. 5/52 for 5 weeks) strategy should be stoploss or delta
    
    NumObs      = (mat - t0)/Dt                     # number of observations
    ObsDensity  = steps/NumObs                      # observation density
    Obs         = seq(1, length(S), ObsDensity)     # observation points
    SObs        = S[Obs]                            # stock price observations
    tauObs      = mat - t[Obs]                      # times to maturity
    DiscFactor  = exp(-r * t[Obs])                  # discounting factor
    
    if (strategy == "stoploss") {
        # Compute hedging costs for stop-loss hedge strategy
        hedge.costs                       = (SObs[1:NumObs] &lt; K) * (SObs[2:(NumObs + 1)] &gt; K) * SObs[2:(NumObs + 1)] - (SObs[1:NumObs] &gt; 
            K) * (SObs[2:(NumObs + 1)] &lt; K) * SObs[2:(NumObs + 1)]
        hedge.costs[length(hedge.costs)]  = hedge.costs[length(hedge.costs)] * (SObs[NumObs + 1] &lt; K) + (hedge.costs[length(hedge.costs)] - 
            K) * (SObs[NumObs + 1] &gt; K)
        disc.hedge.costs                  = hedge.costs * DiscFactor[2:(NumObs + 1)]
        cum.hedge.costs                   = c(0, cumsum(disc.hedge.costs))
    } else if (strategy == "delta") {
        # Compute hedging costs for delta hedge strategy
        y                                 = (log(SObs/K) + (r - sig^2/2) * tauObs)/(sig * sqrt(tauObs))
        delta                             = pnorm(y + sig * sqrt(tauObs))
        hedge.costs                       = c(delta[1] * SObs[1], (delta[2:(NumObs + 1)] - delta[1:NumObs]) * SObs[2:(NumObs + 1)])
        hedge.costs[length(hedge.costs)]  = hedge.costs[length(hedge.costs)] * (SObs[NumObs + 1] &lt; K) + (hedge.costs[length(hedge.costs)] - 
            K) * (SObs[NumObs + 1] &gt; K)
        disc.hedge.costs                  = hedge.costs * DiscFactor
        cum.hedge.costs                   = cumsum(disc.hedge.costs)
    } else {
        stop("ERROR! Please choose stoploss or delta as strategy")
    }
    result = data.frame(CostsMaturity = cum.hedge.costs[NumObs + 1], SMaturity = SObs[NumObs + 1])
}

# Monte Carlo simulation
set.seed(260)  # in order to make the simulation replicable      
M = 1000  # number of samples
Freq = c(5/52, 4/52, 2/52, 1/52, 0.5/52, 0.25/52)   # periods between observations

StoplossCosts   = numeric(M)                        # predefined zero vector for efficiency
DeltaCosts      = numeric(M)
L.stoploss      = numeric(length(Freq))
L.delta         = numeric(length(Freq))

# Calculations
for (j in 1:length(Freq)) {
    for (k in 1:M) {
        Paths             = GeneratePaths(S0, sig, mat, K, r, t0, steps)
        SL                = HedgeCosts(Paths$Time, Paths$StockPrice, K, r, sig, mat, Freq[j], "stoploss")
        Delta             = HedgeCosts(Paths$Time, Paths$StockPrice, K, r, sig, mat, Freq[j], "delta")
        StoplossCosts[k]  = SL$CostsMaturity
        DeltaCosts[k]     = Delta$CostsMaturity
    }
    L.stoploss[j]         = sqrt(var(StoplossCosts))/C
    L.delta[j]            = sqrt(var(DeltaCosts))/C
}
stoploss            = t(data.frame(Dt = round(Freq, digits = 3), HedgePerformance = round(L.stoploss, digits = 3)))
delta               = t(data.frame(Dt = round(Freq, digits = 3), HedgePerformance = round(L.delta, digits = 3)))
colnames(stoploss)  = c("5 weeks", "4 weeks", "2 weeks", "1 week", "1/2 week", "1/4 week")
colnames(delta)     = c("5 weeks", "4 weeks", "2 weeks", "1 week", "1/2 week", "1/4 week")

print(stoploss)
print(delta)
