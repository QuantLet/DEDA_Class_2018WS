graphics.off()
rm(list = ls(all = TRUE))
# setwd('C:/...')

x       = read.table("fx_dem_gbp_usd.dat")
x       = as.matrix(x)
returns = diff(log(x))
t       = seq(23, nrow(x), by = 4 * 257)
 

plot(returns[, 1], returns[, 2], col = "blue", xlab = "DEM/USD", ylab = "GBD/USD", 
    xlim = c(-0.045, 0.04) , ylim = c(-0.045, 0.04), pch = 20)
