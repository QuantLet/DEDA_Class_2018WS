# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("dplyr", "lubridate", "quantmod")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {install.packages(x)})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# Inputs
h     = 250
alpha = 0.2
gamma = 0.94

# Load Data
data = read.csv("SFEVaRbank.csv")

# Functions
calcRMA = function(serie, h = 250){
  # find the first non NA position
  s = rle(is.na(serie))$length[1]
  # Calculate the RMA of the serie for with the parameter h
  rma = sapply((s + h + 1): length(serie), FUN = function(n) {
    sum(serie[(n - h): (n - 1)] * serie[(n - h): (n - 1)], na.rm = T) / (h - 1)})
  rma = c(rep(NA, s + h), rma)
  return(rma)
}

emaWeight = function(h, gamma){
  weight = gamma^(h: 1) * (1 - gamma)
  return(weight)
}

calcEMA = function(serie, gamma = 0.94, h = 250){
  s      = rle(is.na(serie))$length[1]
  weight = emaWeight(h, gamma)
  # ema calculation
  ema = sapply((s + h + 1): length(serie), FUN = function(n) {
    sum(serie[(n - h): (n - 1)] * serie[(n - h): (n - 1)] * weight, na.rm = T)})
  ema = c(rep(NA, s + h), ema)
  return(ema)
}

# Colors
pcolors = data.frame(rma = "blue", ema = "red", stringsAsFactors=FALSE)

# Choose the Stocks
# which(colnames(data) == "LLOYDS.BANKING.GROUP") == 34
Stocks = data[year(data$Date) %in% c(2007, 2008, 2009), c(1, 34)]

# Build portfolio of log-returns
Portfolio = data.frame(Date       = as.Date(Stocks$Date), 
                       Price      = (Stocks[, 2]),
                       PercChange = c(NA,diff(log(Stocks[, 2]))),
                       AbsChange  = c(NA,diff(Stocks[, 2])))

# calculate t-value
tCrit = qnorm(1 - alpha) 
# calculate VaR
Portfolio$VaRRMAY = calcRMA(Portfolio$PercChange, h = h) ^ 0.5 * tCrit
Portfolio$VaREMAY = calcEMA(Portfolio$PercChange, gamma = gamma, h = h) ^ 0.5 * tCrit
Portfolio$VaRRMAL = Portfolio$VaRRMAY * Portfolio$Price
Portfolio$VaREMAL = Portfolio$VaREMAY * Portfolio$Price
# calculate exceedences
Portfolio$RugsRMAL = (Portfolio$AbsChange &gt; Portfolio$VaRRMAL | 
                        Portfolio$AbsChange &lt; -Portfolio$VaRRMAL)
Portfolio$RugsEMAL = (Portfolio$AbsChange &gt; Portfolio$VaREMAL | 
                        Portfolio$AbsChange &lt; -Portfolio$VaREMAL)
Portfolio$RugsRMAY = (Portfolio$PercChange &gt; Portfolio$VaRRMAY | 
                        Portfolio$PercChange &lt; -Portfolio$VaRRMAY)
Portfolio$RugsEMAY = (Portfolio$PercChange &gt; Portfolio$VaREMAY | 
                        Portfolio$PercChange &lt; -Portfolio$VaREMAY)
# remove rows without VaR
Portfolio = Portfolio[- c(1 :h), ]


# Plot qqplots
qqnorm(Portfolio$AbsChange/Portfolio$VaRRMAL, main = "VaR (RMA) Reliability (2008 - 2009)",
       xlab = "Theoretical Quantiles", ylab = "P&amp;L over VaR Quantiles", ylim = c(-3, 3))
qqline(Portfolio$AbsChange/Portfolio$VaRRMAL)
dev.print(device = png, filename = 'VaRqqplotRMA.png', width = 500, height = 500)
# dev.off()
qqnorm(Portfolio$AbsChange/Portfolio$VaREMAL, main = "VaR (EMA) Reliability (2008 - 2009)",
       xlab = "Theoretical Quantiles", ylab = "P&amp;L over VaR Quantiles", ylim = c(-3, 3))
qqline(Portfolio$AbsChange/Portfolio$VaREMAL)
dev.print(device = png, filename = 'VaRqqplotEMA.png', width = 500, height = 500)
# dev.off()


################################################################################
### BACKTESTING PART
################################################################################

################################################################################
### BACKTESTING - Expected Shortfall


# Calculates sigmas RMA/EMA for percentage and absolute changes
Portfolio$sigmaRMAY = Portfolio$VaRRMAY / tCrit
Portfolio$sigmaEMAY = Portfolio$VaREMAY / tCrit
Portfolio$sigmaRMAL = Portfolio$sigmaRMAY * Portfolio$Price
Portfolio$sigmaEMAL = Portfolio$sigmaEMAY * Portfolio$Price

# Define -Zt  for Expected Shortfall
Portfolio$ZRMA = -(Portfolio$AbsChange / (Portfolio$sigmaRMAL))
Portfolio$ZEMA = -(Portfolio$AbsChange / (Portfolio$sigmaEMAL))



################################################################################
#### OPTION: REMOVE OUTLIERS 
  # hist(Portfolio$AbsChange)
  # nrow(Portfolio[abs(Portfolio$AbsChange)&gt;20,])
  # nrow(Portfolio)
### Delete 10 outliers with abs(Lt) above 20
# Portfolio = subset(Portfolio, abs(AbsChange)&lt;=20)


################################################################################
### BACKTESTING TABLES for Tail-VaR 80% 
### assumption: Zt ~ N(0,1)


ALPHA    = 0.20
Un       = qnorm(1 - ALPHA)

ESn      = dnorm(Un, mean = 0, sd = 1, log = FALSE) / (ALPHA) 
sigmaESn = sqrt(1 + Un*ESn - (ESn)^2)


# Occurences of Losses exceeding VaR
Portfolio$OutZRMAn =  ifelse (Portfolio$ZRMA &gt; Un,
                                Portfolio$ZRMA, 
                                NA) 
Portfolio$OutZEMAn =  ifelse (Portfolio$ZEMA &gt; Un,
                                Portfolio$ZEMA, 
                                NA)


# Calculate Expected Shortfall
NRMAn = nrow(Portfolio[!is.na(Portfolio$OutZRMAn), ])
NEMAn = nrow(Portfolio[!is.na(Portfolio$OutZEMAn), ])

ESRMAn = sum(Portfolio$OutZRMAn, na.rm = TRUE) / NRMAn
ESEMAn = sum(Portfolio$OutZEMAn, na.rm = TRUE) / NEMAn

sigmaESRMAn = sqrt(abs(1 + Un * ESRMAn - (ESRMAn)^2))
sigmaESEMAn = sqrt(abs(1 + Un * ESEMAn - (ESEMAn)^2))


# Hypothesis testing for ESn =1.4 (Expected Shortfall = 1.4)
testRMAn = sqrt(NRMAn) * ((ESRMAn - ESn)/sigmaESRMAn)
testEMAn = sqrt(NEMAn) * ((ESEMAn - ESn)/sigmaESEMAn)

signRMAn = 1 - pnorm(testRMAn, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
signEMAn = 1 - pnorm(testEMAn, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)



################################################################################
### BACKTESTING TABLES for Tail-VaR 80% 
### assumption: Zt ~ standard Student's t distribution with df=20
### Here: don not use correction factor for unbiased st. dev. estimator sqrt((df-2)/df), because the biased one was calculated

v  = 20
Ut = qt(1 - ALPHA, df = v)

# for CTE (Expected Shortfall) use closed form solution for Student's t distribution
ESt = dt(Ut, df = v) * ((v + Ut^2) / (v-1)) / (ALPHA)  #1.47

# for CTV use numerical approach
f        = function(x) {x^2 * dt(x, df = v)}
CTEx2t   = integrate(f, lower=Ut, upper=Inf)
CTVt     = CTEx2t$value / (ALPHA) - ESt^2
sigmaESt = sqrt(CTVt)


# Occurences of Losses exceeding VaR
Portfolio$OutZRMAt =  ifelse (Portfolio$ZRMA &gt; Ut,
                                  Portfolio$ZRMA, 
                                  NA) 
Portfolio$OutZEMAt =  ifelse (Portfolio$ZEMA &gt; Ut,
                                  Portfolio$ZEMA, 
                                  NA)


# Calculate Expected Shortfall
NRMAt = nrow(Portfolio[!is.na(Portfolio$OutZRMAt), ])
NEMAt = nrow(Portfolio[!is.na(Portfolio$OutZEMAt), ])

ESRMAt = sum(Portfolio$OutZRMAt, na.rm = TRUE) / NRMAt
ESEMAt = sum(Portfolio$OutZEMAt, na.rm = TRUE) / NEMAt

#sigmaESRMAt = sqrt(abs(1 + Ut*ESRMAt - (ESRMAt)^2))
#sigmaESEMAt = sqrt(abs(1 + Ut*ESEMAt - (ESEMAt)^2))

# Hypothesis testing for ESn =1.47 (Expected Shortfall = 1.47)
  #testRMAt = sqrt(NRMAt) * ((ESRMAt - ESt)/sigmaESRMAt)
  #testEMAt = sqrt(NEMAt) * ((ESEMAt - ESt)/sigmaESEMAt)

testRMAt = sqrt(NRMAn) * ((ESRMAn - ESt) / sigmaESRMAn)
testEMAt = sqrt(NEMAn) * ((ESEMAn - ESt) / sigmaESEMAn)

signRMAt = 1 - pnorm(testRMAt, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
signEMAt = 1 - pnorm(testEMAt, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)


#################################################################################
### Standard Deviation for variable L(t+1)/VaR(t) see in the book: formula (16.19) 
sdRMA = sd(Portfolio$ZRMA / tCrit, na.rm = TRUE)
sdEMA = sd(Portfolio$ZEMA / tCrit, na.rm = TRUE)
sdN   = 1/tCrit

(sdRMA-sdN) / sdN
(sdEMA-sdN) / sdN


################################################################################
### Grid search:
### Investigate QQ-plots to fit better distribution than N(0,1) because of fat tails
### Leptocurtic distribution
### The best fit for standard t-Student with df=4 

y1 = -(Portfolio$ZRMA)
y2 = -(Portfolio$ZEMA)


qqplot(rt(500, df = 4),
		y1,
		ylim = c(-3, 3),
		xlim = c(-3, 3))
qqline(y1, col = 2)
dev.print(device = png, filename = 'VaRqqplot_fattail1.png', width = 500, height = 500)
# dev.off()


qqplot(rt(500, df = 4), 
		y2,
		ylim = c(-3, 3),
		xlim = c(-3, 3))
qqline(y2, col = 2)
dev.print(device = png, filename = 'VaRqqplot_fattail2.png', width = 500, height = 500)
# dev.off()

################################################################################
