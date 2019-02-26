# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("QRMlib")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)


n0 = 10
i = n0
n = 10000/n0
flag = 1

m = n/n0

dat1 = dat2 = matrix(, 100, 2)

for (i in 1:m) {
    
    dat1[i, 1] = i * 10
    dat1[i, 2] = max(rnorm(i, mean = 0, sd = 1))  # for normal distributions
    
    dat2[i, 1] = i * 10
    dat2[i, 2] = max(rstable(i, 1.1, 0))  # for stable distributions
}

plot(dat1[, 2], xlab = ("n"), ylab = ("M(n)"), col = "blue")
title("Limit of M(n) for normal cdf")

dev.new()
plot(dat2[, 2], xlab = ("n"), ylab = ("M(n)"), col = "blue")
title("Limit of M(n) for stable cdf")
