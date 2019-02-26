###################################################################
## Code to replicate the results in the Barunik and Kley (2018)
###################################################################
##
## This script is to estimate a QVAR(1) model to then
## simulate the corresponding quantile coherencies.
##
## Code also replicates Figures 6-8
##
## Authors: Jozef Barunik, and Tobias Kley (2018)
###################################################################

# NOTE: this code produces eight pdf figures:
#
#         - fig_07.pdf
#         - fig_06a1.pdf
#         - fig_06a2.pdf
#         - fig_06a3.pdf
#         - fig_08.pdf
#         - fig_06b1.pdf
#         - fig_06b2.pdf
#         - fig_06b3.pdf
#
#       that are saved to the current working folder.


# load the auxiliary files
source("quantile_coherency_replication_pack.R")

# set seed for RNG
set.seed(1234)

R &lt;- 1000

le &lt;- 1024
quantile &lt;- seq(0.05, 0.95, 0.05)
which_taus &lt;- 2*(1:49) / 100


# load data
ffdata &lt;- read.csv(file("12_Industry_Portfolios_Daily1.csv"),
                   header = TRUE, sep = ";")
ffdatamkt &lt;- read.csv(file("F-F_Research_Data_Factors_daily.csv"),
                      header = TRUE, sep = ";")


###############################################################################
## PREPROCESS THE DATA
###############################################################################

datY10 &lt;- ffdata[,2]
datY20 &lt;- ffdatamkt[,2]

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

Y &lt;- matrix(c(datY1, datY2), ncol=2)
n &lt;- dim(Y)[1]

Y &lt;- as.data.frame(Y)

###############################################################################
## FIT QVAR(1), without spatial dependence
###############################################################################

## model:

# Y_{1,t} = theta_10 + theta_11 Y_{1,t-1} + theta_12 Y_{2,t-1}
# Y_{2,t} = theta_20 + theta_21 Y_{1,t-1} + theta_22 Y_{2,t-1}

n &lt;- nrow(Y) - 1
data1 &lt;- cbind(Y[2:23385, 1],  # Y_{1,t}
    rep(1, n),     rep(0, n),  # theta_10, theta_20
    Y[1:23384, 1], rep(0, n),  # theta_11, theta_21
    Y[1:23384, 2], rep(0, n)   # theta_12, theta_22
)
data2 &lt;- cbind(Y[2:23385, 2],  # Y_{2,t}
    rep(0, n), rep(1, n),      # theta_10, theta_20
    rep(0, n), Y[1:23384, 1],  # theta_11, theta_21
    rep(0, n), Y[1:23384, 2]   # theta_12, theta_22
)
data &lt;- rbind(data1, data2)

colnames(data) &lt;- c("Yi", "th10", "th20", "th11", "th21", "th12", "th22")
data &lt;- data.frame(data)

f_joint &lt;- coef(rq(Yi ~ 0 + th10 + th20 + th11 + th21 + th12 + th22,
        data = data, tau = which_taus))

th10 &lt;- function(u) {
  return(f_joint[1, closest.pos(which_taus, u)])
}
th11 &lt;- function(u) {
  return(f_joint[3, closest.pos(which_taus, u)])
}
th12 &lt;- function(u) {
  return(f_joint[5, closest.pos(which_taus, u)])
}
th20 &lt;- function(u) {
  return(f_joint[2, closest.pos(which_taus, u)])
}
th21 &lt;- function(u) {
  return(f_joint[4, closest.pos(which_taus, u)])
}
th22 &lt;- function(u) {
  return(f_joint[6, closest.pos(which_taus, u)])
}

## now simulate the qSD
myCop &lt;- indepCopula()
model &lt;- function (n) {
  rqvar1(n, th10, th11, th12, th20, th21, th22,
      myCop, overhead = 1000)
}
qSD &lt;- quantileSD(le, ts = model, levels.1 = quantile, R = R)
save(qSD, f_joint, th10, th11, th12, th20, th21, th22,
    file = "QVAR1_no_spatial.Rdata")

###############################################################################
## (4d) FIT QVAR(1), with spatial dependence in Y_2t
###############################################################################

## model:

# Y_{1,t} = theta_10                     + theta_111 Y_{1,t-1} + theta_121 Y_{2,t-1}
# Y_{2,t} = theta_20 + theta_210 Y_{1,t} + theta_211 Y_{1,t-1} + theta_221 Y_{2,t-1}

n &lt;- nrow(Y) - 1
data1 &lt;- cbind(Y[2:23385, 1],  # Y_{1,t}
    rep(1, n),     rep(0, n),  # theta_10, theta_20
    rep(0, n),                 # theta_210
    Y[1:23384, 1], rep(0, n),  # theta_111, theta_211
    Y[1:23384, 2], rep(0, n)   # theta_121, theta_221
)
data2 &lt;- cbind(Y[2:23385, 2],  # Y_{2,t}
    rep(0, n), rep(1, n),      # theta_10, theta_20
    Y[2:23385, 1],             # theta_210
    rep(0, n), Y[1:23384, 1],  # theta_111, theta_211
    rep(0, n), Y[1:23384, 2]   # theta_121, theta_221
)
data &lt;- rbind(data1, data2)

colnames(data) &lt;- c("Yi", "th10", "th20", "th210",
    "th111", "th211", "th121", "th221")
data &lt;- data.frame(data)

f_joint &lt;- coef(rq(Yi ~ 0 + th10 + th20 + th210
            + th111 + th211 + th121 + th221,
        data = data, tau = which_taus))

th10 &lt;- function(u) {
  return(f_joint[1, closest.pos(which_taus, u)])
}
th20 &lt;- function(u) {
  return(f_joint[2, closest.pos(which_taus, u)])
}
th210 &lt;- function(u) {
  return(f_joint[3, closest.pos(which_taus, u)])
}
th111 &lt;- function(u) {
  return(f_joint[4, closest.pos(which_taus, u)])
}
th211 &lt;- function(u) {
  return(f_joint[5, closest.pos(which_taus, u)])
}
th121 &lt;- function(u) {
  return(f_joint[6, closest.pos(which_taus, u)])
}
th221 &lt;- function(u) {
  return(f_joint[7, closest.pos(which_taus, u)])
}

## now simulate the qSD
myCop &lt;- indepCopula()
model &lt;- function (n) {
  rqvar2b(n, th10, th111, th121, th20, th210, th211, th221,
      myCop, overhead = 1000)
}
qSD &lt;- quantileSD(le, ts = model, levels.1 = quantile, R = R)
save(qSD, f_joint, th10, th111, th121, th20, th210, th211, th221,
    file = "QVAR1_Y2_spatial.Rdata")

###############################################################################
## Generate figures 6, 7, and 8
###############################################################################

# clean global environment
rm(list = ls())

#set thickness
tht=1.5

which_taus &lt;- 2*(1:49) / 100

# parameters for the pdfs
le &lt;- 1024
quantile &lt;- seq(0.05, 0.95, 0.05)
quantilelegend &lt;- c("0.5 | 0.5","0.05 | 0.05","0.95 | 0.95")

lim1 &lt;- 0
lim2 &lt;- 1
lim3 &lt;- -0.4
lim4 &lt;- 0.2


###############################################################################
## QVAR, no spatial dependence
###############################################################################

load("QVAR1_no_spatial.Rdata")

# (i) Plot parameter functions

pdf("fig_07.pdf", width = 9, height = 5)
  # plot the functions theta
  u &lt;- which_taus
  
  op &lt;- par(mfrow = c(2,3), mar = c(4, 2, 3, 0.5) + 0.1)
  
  plot(x = u, th10(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{10}(\\tau)$"))
  plot(x = u, th11(u), type = "l", xlab=expression(tau), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{11}(\\tau)$"))
  plot(x = u, th12(u), type = "l", xlab=expression(tau), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{12}(\\tau)$"))
  plot(x = u, th20(u), type = "l", xlab=expression(tau), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{20}(\\tau)$"))
  plot(x = u, th21(u), type = "l", xlab=expression(tau), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{21}(\\tau)$"))
  plot(x = u, th22(u), type = "l", xlab=expression(tau), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{22}(\\tau)$"))
  par(op)
dev.off()

# (ii) Quantile coherency (for 4 different combinations of parameters
#                              and all frequencies)

V &lt;- getValues(qSD, levels.1 = c(0.5, 0.05, 0.95), levels.2 = c(0.5, 0.05, 0.95))
V &lt;- V[1:(dim(V)[1]/2),,,,]

frequencies &lt;- (0:le)/(le+1)
freq &lt;- frequencies[which(frequencies != 0)][1:(le/2)]

# QVAR1_no_spatial_1.pdf
pdf("fig_06a1.pdf", width=4, height=3.5)
  par(mar=c(4,2,2,0.5)+0.1)
  
  plot(x = freq, xlim = c(0,0.5), ylim = c(lim3, lim2), type = "l",
       xlab = expression(omega / 2 * pi), ylab = "")
  for (i1 in 1:3) {
    lines(x = freq, y = Re(V[,1,i1,2,i1] / sqrt(V[,1,i1,1,i1] * V[,2,i1,2,i1])),
          col = "black", lty = i1, lwd = tht)
  }
  axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
  abline(v = c(1/5, 1/22, 1/250), col = "gray")
  legend("bottom", inset = .03, "center",
         c("0.5 | 0.5", "0.05 | 0.05", "0.95 | 0.95"), cex = 0.65,
         lwd = c(1, 1, 1), lty = c(1:3), horiz = TRUE, bg = "white")

dev.off()

pdf("fig_06a2.pdf", width = 4, height = 3.5)
  par(mar = c(4, 2, 2, 0.5) + 0.1)
  plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.2, lim2), type = "l",
       xlab = expression(omega / 2 * pi), ylab = "")
  lines(x = freq, y = Re(V[,1,1,2,3] / sqrt(V[,1,1,1,1] * V[,2,3,2,3])),
        col = "black", lty = i1, lwd = tht)
  axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
  abline(v = c(1/5, 1/22, 1/250), col = "gray")
  legend("bottom", inset = .03, "center", c("0.05 | 0.95"), cex = 0.8,
         lwd = c(1, 1, 1), lty = c(1:3), horiz = TRUE, bg = "white")
dev.off()

# (iii) Quantile coherency (for 3 frequencies and all taus)

V &lt;- getValues(qSD, frequencies = 2 * pi * c(1/5, 1/22, 1/250))

pdf("fig_06a3.pdf", width = 4, height = 3.5)
  
  par(mar = c(4, 2, 2, 0.5) + 0.1)
  
  plot(x = freq, xlim = c(0, 1), ylim = c(lim3, lim2), type = "n",
      xlab = expression(tau), ylab = "")
  
  for (i in 1:3) {   
    qcoh &lt;- rep(0, length(quantile))
    for (j in 1:length(quantile)) {
      qcoh[j] &lt;- Re(V[i, 1, j, 2, j] / sqrt(V[i, 1, j, 1, j] * V[i, 2, j, 2, j]))
    }
    lines(x = quantile, y = qcoh, col = "black", lty = i, lwd = tht)
  }
  legend("bottom", inset = .03, "center", c("W", "M", "Y"), cex = 0.65,
         lwd=c(1, 1, 1), lty = c(1:3), horiz = TRUE, bg = "white")

dev.off()



###############################################################################
## QVAR spatial 
###############################################################################

load("QVAR1_Y2_spatial.Rdata")

pdf("fig_08.pdf", width = 12, height = 5)
  # plot the functions theta
  u &lt;- which_taus
  par(mar = c(4, 2, 3, 0.5) + 0.1)
  
  op &lt;- par(mfrow = c(2,4))
  
  plot(x = u, th10(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{10}(\\tau)$"))
  plot.new()
  plot(x = u, th111(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{111}(\\tau)$"))
  plot(x = u, th121(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{121}(\\tau)$"))
  
  plot(x = u, th20(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{20}(\\tau)$"))
  plot(x = u, th210(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{210}(\\tau)$"))
  plot(x = u, th211(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{211}(\\tau)$"))
  plot(x = u, th221(u), type = "l", xlab=TeX("$\\tau$"), ylab = "", lwd = tht)
  title(TeX("$\\hat{\\theta}_{221}(\\tau)$"))
  
  par(op)
dev.off()

# (ii) Quantile coherency (for 4 different combinations of parameters
#                              and all frequencies)

V &lt;- getValues(qSD, levels.1 = c(0.5, 0.05, 0.95), levels.2 = c(0.5, 0.05, 0.95))
V &lt;- V[1:(dim(V)[1]/2),,,,]

frequencies &lt;- (0:le)/(le+1)
freq &lt;- frequencies[which(frequencies != 0)][1:(le/2)]

# QVAR1_Y2_spatial_1.pdf
pdf("fig_06b1.pdf", width=4, height=3.5)
  par(mar = c(4, 2, 2, 0.5) + 0.1)
  
  plot(x = freq, xlim = c(0, 0.5), ylim = c(0.2, lim2), type="l",
       xlab = expression(omega / 2 * pi), ylab = "")
  for (i1 in 1:3) {
    lines(x = freq, y = Re(V[,1,i1,2,i1] / sqrt(V[,1,i1,1,i1] * V[,2,i1,2,i1])),
          col = "black", lty = i1, lwd = tht)
  }
  axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
  abline(v = c(1/5, 1/22, 1/250), col = "gray")
  legend("bottom", inset = .03, "center",
         c("0.5 | 0.5", "0.05 | 0.05", "0.95 | 0.95"), cex = 0.65,
         lwd=c(1, 1, 1), lty = c(1:3), horiz = TRUE, bg = "white")

dev.off()

pdf("fig_06b2.pdf", width = 4, height = 3.5)
  par(mar = c(4, 2, 2, 0.5) + 0.1)
  plot(x = freq, xlim = c(0, 0.5), ylim = c(-0.2, lim2), type = "l",
       xlab = expression(omega / 2 * pi), ylab = "")
  lines(x = freq, y = Re(V[,1,1,2,3] / sqrt(V[,1,1,1,1] * V[,2,3,2,3])),
        col = "black", lty = i1, lwd = tht)
  axis(side = 3, at = c(1/5, 1/22, 1/250), labels = c("W", "M", "Y"))
  abline(v=c(1/5, 1/22, 1/250), col = "gray")
  legend("bottom", inset = .03, "center", c("0.05 | 0.95"), cex = 0.8,
         lwd = c(1,1,1), lty = c(1:3), horiz = TRUE, bg = "white")
dev.off()

# (iii) Quantile coherency (for 3 frequencies and all taus)

V &lt;- getValues(qSD, frequencies = 2 * pi * c(1/5, 1/22, 1/250))

pdf("fig_06b3.pdf", width = 4, height = 3.5)
  
  par(mar = c(4, 2, 2, 0.5) + 0.1)
  
  plot(x = freq, xlim = c(0, 1), ylim = c(0.3, lim2), type = "n",
      xlab = expression(tau), ylab = "")
  
  for (i in 1:3) {   
    qcoh &lt;- rep(0, length(quantile))
    for (j in 1:length(quantile)) {
      qcoh[j] &lt;- Re(V[i, 1, j, 2, j] / sqrt(V[i, 1, j, 1, j] * V[i, 2, j, 2, j]))
    }
    lines(x = quantile, y = qcoh, col = "black", lty = i, lwd = tht)
  }
  legend("bottom", inset = .03, "center", c("W", "M", "Y"),
      cex = 0.65, lwd = c(1, 1, 1),
      lty = c(1:3), horiz = TRUE, bg = "white")

dev.off()
