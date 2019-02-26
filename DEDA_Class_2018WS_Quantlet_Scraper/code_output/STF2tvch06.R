rm(list = ls(all = TRUE))
graphics.off()

# install.packages(c('aws', 'fGarch', 'igraph', 'Hmisc'))
library("aws")
library("fGarch")
library("igraph")
library("Hmisc")

data &lt;- read.delim2("SP1997-2005s.txt")

time &lt;- (1:length(data[, 1]))
res &lt;- aws(data[, 1], family = "Gaussian")
dat0 &lt;- data[, 1] - c(mean(data[, 1]))
dat0 &lt;- dat0/sd(dat0)

ladj &lt;- 0.16

p &lt;- 1
n &lt;- length(dat0)
matX &lt;- NULL
for (s in (1:p)) matX &lt;- cbind(matX, c(double(p), dat0[(p - s + 1):(n - s)]^2))

pred &lt;- 0 * time - 1
esterr &lt;- pred
ghist &lt;- 250
ggarch &lt;- pred
h &lt;- 1
for (i in 1076:2088) {
    print(i)
    res &lt;- aws(dat0[(i - 500):(i - 1)], family = "Volatility", ladjust = ladj, demo = FALSE)
    awspred &lt;- awsdata(res, "theta")
    pred[i] &lt;- awspred[length(awspred)]
    esterr[i] &lt;- sum(abs(pred[i] - dat0[i:(i + h - 1)]^2))
    
    gest &lt;- garchFit(~garch(1, 1), data = dat0[1:(i - 1)], trace = FALSE, include.mean = FALSE)
    ggarch[i] &lt;- sum(abs(predict(gest, n.ahead = h)$standardDeviation^2 - dat0[i:(i + h - 1)]^2))
    
    print(c(pred[i], dat0[i:(i + h - 1)]^2, esterr[i], ggarch[i], awspred[length(awspred), ]))
}

timet &lt;- (time - 1078)/250 + 2001
dev.new()
plot(timet[pred &gt;= 0], dat0[pred &gt;= 0]^2, cex = 0.1, xaxp = c(2001, 2005, 4), xlab = "Time", ylab = "Squared log-returns")
lines(timet[pred &gt;= 0], pred[pred &gt;= 0])
minor.tick(4, 5)
readline("Save the plot...")

lc &lt;- lc[1:sum(pred &gt;= 0)]
time &lt;- time[1:sum(pred &gt;= 0)]

timet &lt;- timet[pred &gt;= 0]
errs &lt;- esterr[pred &gt;= 0]
ggarch &lt;- ggarch[pred &gt;= 0]
pred &lt;- pred[pred &gt;= 0]

lags &lt;- 21
dev.new()
plot(timet[lags:length(time)], running.mean(errs, lags)/running.mean(ggarch, lags), type = "l", xlab = "Time", ylab = "MAPE rel. to GARCH(1,1)", 
    xaxp = c(2001, 2005, 4))
abline(1, 0, lty = "dotted")
minor.tick(4, 5)
print(mean(errs))
print(mean(ggarch))
readline("Save the plot...")

print("Mean absolute forecast errors of AWS and GARCH:")

print("By year:")
for (ye in 1:4) print(c(mean(errs[(250 * (ye - 1) + 1):(250 * ye)]), mean(ggarch[(250 * (ye - 1) + 1):(250 * ye)])))
print("Total:")
print(c(mean(errs[1:1000]), mean(ggarch[1:1000])))
