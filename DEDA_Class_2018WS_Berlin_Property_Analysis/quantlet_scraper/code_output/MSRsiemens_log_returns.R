graphics.off()
rm(list = ls(all = TRUE))
#setwd("C:/...") 

x = read.table("siemens_log_returns.dat")

plot(x[, 2], xlim = c(-1, length(x[, 2]) + 1), ylim = c(-0.15, 0.15), 
    main = "Daily Log-Returns of Siemens", type = "l", col = "blue", xlab = "", ylab = "")
