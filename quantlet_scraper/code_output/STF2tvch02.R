# Close windows and clear variables
rm(list = ls(all = TRUE))
graphics.off()

# Load library install.packages(c('aws', 'fGarch', 'igraph', 'Hmisc'))


library("fGarch")
library("igraph")
library("stats")
library("Hmisc")

# Please change working directory setwd('C:/')
data &lt;- read.delim2("SP1997-2005s.txt")

time &lt;- (1:length(data[, 1]))
dat0 &lt;- data[, 1] - c(mean(data[, 1]))
dat0 &lt;- dat0/sd(dat0)

h &lt;- 1
pred &lt;- 0 * time - 1

for (i in 1076:2088) {
    # print(i)
    
    gest &lt;- garchFit(~garch(1, 1), data = dat0[1:(i - 1)], trace = FALSE, include.mean = FALSE)
    pred[i] &lt;- predict(gest, n.ahead = h)$standardDeviation^2
}

timet &lt;- (time - 1078)/250 + 2001
plot(timet[pred &gt;= 0], dat0[pred &gt;= 0]^2, cex = 0.2, xaxp = c(2001, 2005, 4), xlab = "Time", ylab = "Squared log-returns")
lines(timet[pred &gt;= 0], pred[pred &gt;= 0])
