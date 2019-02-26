###################################################################
## Code to replicate the results in the Barunik and Kley (2018)
###################################################################
##
## Code to Figure 3
##
## Authors: Jozef Barunik, and Tobias Kley (2018)
###################################################################

# NOTE: this code produces two pdf figures (fig_03a.pdf, fig_03b.pdf)
#       that are saved to the current working folder.


# load the auxiliary files
source("quantile_coherency_replication_pack.R")

# set seed for RNG
set.seed(1234)

# Load the data
ffdata &lt;- read.csv(file("12_Industry_Portfolios_Daily1.csv"),
                        header = TRUE, sep = ";")
ffdatamkt &lt;- read.csv(file("F-F_Research_Data_Factors_daily.csv"),
                           header = TRUE, sep = ";")

datY10 &lt;- ffdata[, 2]
datY20 &lt;- ffdatamkt[, 2]

# remove mean and heteroscedasticity
model &lt;- ugarchspec(variance.model = list(garchOrder = c(1,1)),
                    mean.model = list(armaOrder = c(1,1), include.mean = T),
                    distribution.model = "norm")

fit1  &lt;- ugarchfit(model, data = datY10)
fit2  &lt;- ugarchfit(model, data = datY20)
datY1 &lt;- fit1@fit$residuals / fit1@fit$sigma
datY2 &lt;- fit2@fit$residuals / fit2@fit$sigma
datY1 &lt;- datY1 - mean(datY1)
datY2 &lt;- datY2 - mean(datY2)
 
Y &lt;- matrix(c(datY1, datY2), ncol = 2)
n &lt;- dim(Y)[1]

## Compute Quantile cross-spectral densities on data

# chose quantile levels
quantile &lt;- c(0.5, 0.05, 0.95)
sPG &lt;- smoothedPG(Y, levels.1 = quantile,
                  weight = kernelWeight(W = W1, b = 0.5 * n^(-1/4)))

# Choose frequencies
cut = (0:123) / 124
cut = round(cut * 23385) / 23385 

# Compute Quantile-Coherency and confidence intervals
Coh &lt;- getCoherency(sPG, frequencies = 2 * pi * cut)

# NOTE: this takes few minutes
CI &lt;- getPointwiseCIs(sPG, quantity = "coherency", frequencies = 2 * pi * cut)

# FIGURE 3 (left part for tau1 = tau2)
pdf(file=paste("fig_03a.pdf", sep = ''), width = 8, height = 5.3)

  par(mar = c(4, 2, 5.5, 0.5) + 0.1)
  par(mfrow = c(1, 2))
    
  freq &lt;- cut[1:(length(cut)/2)]
  le &lt;- (length(cut) / 2)
    
  clr &lt;- gray.colors(length(quantile) + 2,start = 0.1, end = 0.8)
  clr &lt;- c(clr[2], clr[1], clr[5])
  d &lt;- c(10, 20, 30)
  a &lt;- c(90, 90, 90)
    
  plot(x = freq, xlim = c(0,0.5), ylim = c(0.1, 0.9), type = "l",
       xlab = expression(omega / 2*pi), ylab = "")
  for (i1 in 1:length(quantile)) {
    polygon(x = c(rev(freq), freq), y = c(rev(Re(CI$lo[1:le,1,i1,2,i1])),
                                              Re(CI$up[1:le,1,i1,2,i1])),
            col = clr[i1], density = d[i1], angle = a[i1],
            lty = 3, lwd = c(0.8, 0.8, 0.8))
  }
    
  for (i1 in 1:length(quantile)) {
    lines(x = freq, y = Re(Coh[1:le,1,i1,2,i1,1]),
          col = "black", lty = i1, lwd = c(1.5, 1.5, 1.5))
  }
  axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
  abline(v = c(1/5, 1/22, 1/250), col = "gray")
  legend("bottom", inset = .03, "center",
         c("0.5 | 0.5", "0.05 | 0.05", "0.95 | 0.95"), cex = 0.65,
         lwd = c(1, 1, 1), lty = c(1:length(quantile)),
         horiz = TRUE, bg = "white")
  
  # FIGURE 3 (middle part for tau1 \ne tau2)
  plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.2, 0.9), type = "l",
       xlab = expression(omega / 2 * pi), ylab = "")
  i1 &lt;- 1
  i2 &lt;- 2
  i3 &lt;- 3
  polygon(x = c(rev(freq), freq), y = c(rev(Re(CI$lo[1:le,1,i2,2,i3])),
                                        Re(CI$up[1:le,1,i2,2,i3])),
          col = clr[i1], density = d[i1], angle = a[i1],
          lty = 3,lwd = c(0.8,0.8,0.8))
  lines(x = freq, y = Re(Coh[1:le,1,i2,2,i3,1]),
        col = "black", lty = i1, lwd = c(1.5, 1.5, 1.5))
  axis(side = 3, at = c(1/5, 1/22, 1/250),
       labels = c("W", "M", "Y"))
  abline(v = c(1/5, 1/22, 1/250), col = "gray")
  legend("bottom", inset = .03, "center", c("0.05 | 0.95"),
         cex = 0.8, lwd = c(1, 1, 1), lty = c(1:length(quantile)),
         horiz = TRUE, bg = "white")
dev.off()



## Compute Quantile cross-spectral densities on data

# choose quantile levels
quantile &lt;- seq(0.05, 0.95, 0.05)
sPG &lt;- smoothedPG(Y, levels.1 = quantile,
                  weight = kernelWeight(W = W1, b = 0.5 * n^(-1/4)))

# Quantile-Coherency
# chose daily, weekly and monthly frequency 
cut=c(1/5, 1/22, 1/250)

# Compute Quantile-Coherency and confidence intervals
Coh &lt;- getCoherency(sPG, frequencies = 2 * pi * cut)
# NOTE: this takes few minutes
CI &lt;- getPointwiseCIs(sPG, quantity = "coherency", frequencies = 2 * pi * cut)

# FIGURE 3 (right part across frequencies)
pdf(file = paste("fig_03b.pdf", sep = ""), width = 4, height = 5.3)
  par(mar = c(4, 2, 5.5, 0.5) + 0.1)
  
  plot(x = quantile, xlim = c(0.05, 0.95), ylim = c(0.3, 0.9), type = "l",
       xlab = TeX("$\\tau_1 = \\tau_2$"), ylab = "")
  for (i1 in 1:3) {
    polygon(x = c(rev(quantile), quantile),
            y = c(rev(diag(Re(CI$lo[i1,1,,2,]))), diag(Re(CI$up[i1,1,,2,]))),
            col = clr[i1], density = d[i1], angle = a[i1],
            lty = 3,lwd = c(0.8, 0.8, 0.8))
  }
    
  for (i1 in 1:3) {
    lines(x = quantile, y = diag(Re(Coh[i1,1,,2,,1])),
          col = "black", lty = i1, lwd = c(1.5, 1.5, 1.5))
    }
  legend("bottom", inset = .03, "center", c("W", "M", "Y"),
         cex = 0.65, lwd = c(1, 1, 1), lty = c(1:length(quantile)),
         horiz = TRUE, bg = "white")
dev.off()
