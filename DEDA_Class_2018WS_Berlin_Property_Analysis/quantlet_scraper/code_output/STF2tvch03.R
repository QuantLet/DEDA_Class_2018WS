# Close windows and clear variables
graphics.off()
rm(list = ls(all = TRUE))

# Load library install.packages(c('aws', 'fGarch', 'igraph', 'Hmisc'))
install.packages("fGarch")
library("fGarch")
library("igraph")
library("stats")
library("Hmisc")

# Please change working directory setwd('C:/...')
data &lt;- read.delim2("SP1997-2005s.txt")

time &lt;- (1:length(data[, 1]))
dat0 &lt;- data[, 1] - c(mean(data[, 1]))
dat0 &lt;- dat0/sd(dat0)

p &lt;- 1
pred &lt;- 0 * time - 1
esterr &lt;- pred
h &lt;- 1

ghist &lt;- 250

for (i in 1076:2088) {
    print(i)
    
    gest &lt;- garchFit(~garch(1, 1), data = dat0[(i - ghist):(i - 1)], trace = FALSE, include.mean = FALSE)
    pred[i] &lt;- predict(gest, n.ahead = h)$standardDeviation^2
    esterr[i] &lt;- sum(abs(pred[i] - dat0[i:(i + h - 1)]^2))
}

errs &lt;- esterr[pred &gt;= 0]
print(c("Year 2001:", mean(errs[1:250])))
print(c("Year 2002:", mean(errs[251:500])))
print(c("Year 2003:", mean(errs[501:750])))
print(c("Year 2004:", mean(errs[751:1000])))
print(c("Total:", mean(errs[1:1000])))

timet &lt;- (time - 1078)/250 + 2001
plot(timet[pred &gt;= 0], dat0[pred &gt;= 0]^2, cex = 0.2, xaxp = c(2001, 2005, 4), xlab = "Time", ylab = "Squared log-returns")
lines(timet[pred &gt;= 0], pred[pred &gt;= 0])

minor.tick(4, 5)
