#load and install required packages

pkg = c('plyr', 'TTR')
lapply(pkg, require, character.only = TRUE)
lapply(pkg, library, character.only = TRUE)

#open data sets, set working directory if necessary
#setwd(...)
dax_all_days   = read.csv2('DAX_all_days_aggregate.csv')
dax_press_days = read.csv2('DAX_press_days_aggregate.csv')

#delete old graphic device
dev.off()

#generate plot
plot(dax_press_days$time, dax_press_days$sd, 
     type = 'l', 
     axes = FALSE, 
     xlab = 'Time (CET)', 
     ylab = 'Average volatility', 
     col  = 'darkblue', 
     lwd  = 1)

#transparent background
par(bg = NA)

#box around plot
box(which = 'plot', lty = 'solid')

#ticks and labels
axis(side = 1, at =seq(1, 101, 6), labels = as.character(dax_press_days$time2[seq(1, 101, 6)]))
axis(side = 2, labels = TRUE)

#add vertical lines at 13:45, 14:30, 15:30
lines(dax_all_days$sd, type='l', lty = 2, col = 'darkblue', lwd = 1)
abline(v = c(58,67, 79), col = 'red')
par(bg = NA)
