# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Declare stock price variables
S0    = 49                  # current stock price
sig   = 0.2                 # volatility 

# Declare option pricing variables
r     = 0.05                # interest rate
K     = 50                  # strike price
t0    = 0/52                # current time (1 week = 1/52)
mat   = 20/52               # maturity
tau   = 10/52               # time to maturity

# Black-Scholes option price
X     = seq(40, 60, 0.5)    # range for the stock price
y     = (log(X/K) + (r - (sig^2)/2) * tau)/(sig * sqrt(tau))
C     = X * pnorm(y + sig * sqrt(tau)) - exp(-r * tau) * K * pnorm(y)

# Delta at point X[x]
x     = round(length(X)/2 + 2)
Delta = pnorm(y[x] + sig * sqrt(tau))

# Plot
plot(X, C, type = "l", xlab = "StockPrice", ylab = "OptionPrice", xaxt = "n", yaxt = "n",
     main = "Delta hedge strategy")
intercept = C[x] - X[x] * Delta
lines(X[(x - 12):(x + 12)], Delta * X[(x - 12):(x + 12)] + intercept, col = "red")
lines(X[1:x], rep(C[x], x), lty = 2)
lines(rep(X[x], x), C[1:x], lty = 2)
axis(1, X[x], "S", las = 1)
axis(2, C[x], "C(S)", las = 1)
text(X[x + 16], C[x + 4], expression(Delta), col = "red")
text(X[x + 11], C[x + 4], "slope = 0.6 =", col = "red", cex = 0.8)

DeltaStrat = data.frame(S = X[x], Delta = Delta, HedgeCosts = Delta * X[x])
print(DeltaStrat)
