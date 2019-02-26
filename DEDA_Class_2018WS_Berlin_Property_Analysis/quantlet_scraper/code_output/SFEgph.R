# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("fracdiff")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
    install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

set.seed(1)
N = 1000
d0 = 0.4

dz1 = fracdiff.sim(N, d = d0)
dz2 = fracdiff.sim(N, d = -d0)

d1 = fdGPH(dz1$series)
d2 = fdGPH(dz2$series)

arfima1 = fracdiff.sim(N, d = d1$d)
arfima2 = fracdiff.sim(N, d = d2$d)

# plot
par(mfrow = c(2, 1))
plot(arfima1$series, type = "l", col = "blue", xlab = "", ylab = "", ylim = c(-5, 
    5), cex.axis = 1.4)
plot(arfima2$series, type = "l", col = "blue", xlab = "", ylab = "", ylim = c(-5, 
    5), cex.axis = 1.4)
