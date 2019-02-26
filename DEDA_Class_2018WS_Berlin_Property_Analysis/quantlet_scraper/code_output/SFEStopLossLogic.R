# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Declare stock price variables
S0    = 49                        # current stock price
sig   = 0.2                       # volatility 

# Declare option pricing variables
r     = 0.05                      # interest rate
K     = 50                        # strike price
t0    = 0/52                      # current time (1 week = 1/52)
mat   = 20/52                     # maturity

# Parameters for calculations
dt    = 0.2/52                    # period between steps
np    = (mat - t0)/dt             # number of periods
t     = seq(t0, mat, dt)          # maturity - t0 divided in n intervals
tau   = mat - t                   # times to maturity

# Simulate first stock price path
set.seed(288)                     # in order to make the simulation replicable
Wt  = c(0, cumsum(sqrt(dt) * rnorm(np)))
S   = S0 * exp((r - 0.5 * sig^2) * t + sig * Wt)

# Calculate buying and selling timepoints
Buy         = (S[1:np] &lt; K) * (S[2:(np + 1)] &gt; K)
Sell        = (S[1:np] &gt; K) * (S[2:(np + 1)] &lt; K)
hedge.costs = numeric(sum(Buy) + sum(Sell) + 1)
buytime     = numeric(sum(Buy))   # predefined zero vector for efficiency
selltime    = numeric(sum(Sell))  # length of buy/sell time is the number of purchases/sales

# Define counting variables for calculations
n   = 1
m   = 1
p   = 1

for (k in 1:length(Buy)) {
  if (Buy[k] == 1) {
    buytime[n] = t[k + 1]
    hedge.costs[p] = S[k + 1]
    n = n + 1
    p = p + 1
  }
  if (Sell[k] == 1) {
    selltime[m] = t[k + 1]
    hedge.costs[p] = -S[k + 1]
    m = m + 1
    p = p + 1
  }
}

hedge.costs[length(hedge.costs)] = -K * (S[np + 1] &gt; K)

# Plot
par(mfrow = c(2, 1))
plot(t, S, type = "l", lty = 5, ylab = "StockPrice", xlab = "Time", xaxt = "n", yaxt = "n",
     main = "Stop-loss strategy - path 1")
lines(t, S, type = "p", pch = 19)
abline(h = 50, lty = 5)
axis(2, K, "K", las = 1)
buyaxis = rep("buy", length(buytime))
sellaxis = rep("sell", length(selltime))
axis(1, buytime, buyaxis)
axis(1, selltime, sellaxis)
abline(v = buytime, lty = 2, lwd = 2, col = "blue")
abline(v = selltime, lty = 2, lwd = 2, col = "red")
if (S[length(S)]&gt;K){
  axis(1, mat, "T: sell")
  abline(v = mat, lty = 2, lwd = 2, col = "red")
}

StopLossStrat1 = data.frame(HedgeCosts = hedge.costs, CumHedgeCosts = cumsum(hedge.costs),
                            row.names = c(1:(length(hedge.costs) - 1), "Maturity"))


# Simulate second stock price path
set.seed(924)                     # in order to make the simulation replicable
Wt    = c(0, cumsum(sqrt(dt) * rnorm(np)))
S     = S0 * exp((r - 0.5 * sig^2) * t + sig * Wt)

# Calculate buying and selling timepoints
Buy         = (S[1:np] &lt; K) * (S[2:(np + 1)] &gt; K)
Sell        = (S[1:np] &gt; K) * (S[2:(np + 1)] &lt; K)
hedge.costs = numeric(sum(Buy) + sum(Sell) + 1)
buytime     = numeric(sum(Buy))   # predefined zero vector for efficiency
selltime    = numeric(sum(Sell))  # length of buy/sell time is the number of purchases/sales

# Define counting variables for calculations
n   = 1
m   = 1
p   = 1

for (k in 1:length(Buy)) {
  if (Buy[k] == 1) {
    buytime[n] = t[k + 1]
    hedge.costs[p] = S[k + 1]
    n = n + 1
    p = p + 1
  }
  if (Sell[k] == 1) {
    selltime[m] = t[k + 1]
    hedge.costs[p] = -S[k + 1]
    m = m + 1
    p = p + 1
  }
}

hedge.costs[length(hedge.costs)] = -K * (S[np + 1] &gt; K)

# Plot
plot(t, S, type = "l", lty = 5, ylab = "StockPrice", xlab = "Time", xaxt = "n", yaxt = "n",
     main = "Stop-loss strategy - path 2")
lines(t, S, type = "p", pch = 19)
abline(h = 50, lty = 5)
axis(2, K, "K", las = 1)
buyaxis = rep("buy", length(buytime))
sellaxis = rep("sell", length(selltime))
axis(1, buytime, buyaxis)
axis(1, selltime, sellaxis)
abline(v = buytime, lty = 2, lwd = 2, col = "blue")
abline(v = selltime, lty = 2, lwd = 2, col = "red")
if (S[length(S)]&gt;K){
  axis(1, mat, "T: sell")
  abline(v = mat, lty = 2, lwd = 2, col = "red")
}

StopLossStrat2 = data.frame(HedgeCosts = hedge.costs, CumHedgeCosts = cumsum(hedge.costs),
                            row.names = c(1:(length(hedge.costs) - 1), "Maturity"))

print(StopLossStrat1)
print(StopLossStrat2)
