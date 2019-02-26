# Clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# Please change working directory setwd('C:/...')

data &lt;- read.delim2("SP1997-2005s.txt")

time &lt;- (1:length(data[, 1]))
dat0 &lt;- data[, 1] - c(mean(data[, 1]))
dat0 &lt;- dat0/sd(dat0)

timet &lt;- (time - 1078)/250 + 2001
plot(timet[time &gt;= 1075], dat0[time &gt;= 1075], xaxp = c(2001, 2005, 4), xlab = "Time", ylab = "Log-returns", type = "l")
