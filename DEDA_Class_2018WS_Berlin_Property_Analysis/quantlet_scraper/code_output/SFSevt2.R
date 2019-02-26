# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("QRMlib")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

n = 100

# Gumbel
gumb1 = rGEV(100, xi = 0, mu = 0, sigma = 1)
gumb2 = sort(gumb1)
gumb = pnorm(gumb2, 0, 1)

t = (1:n)/(n + 1)

dev.new()
plot(gumb, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""))
lines(t, t, type = "l", col = "red", lwd = 2)
title("PP Plot of Extreme Value - Gumbel")

# Frechet
frec1 = rGEV(100, xi = 0.5, sigma = 0.5, mu = 1)
frec2 = sort(frec1)
frec = pnorm(frec2, 0, 1)

t = (1:n)/(n + 1)

dev.new()
plot(frec, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""), 
    xlim = c(0, 1), ylim = c(0, 1))
lines(t, t, type = "l", col = "red", lwd = 2)
title("PP Plot of Extreme Value - Frechet")

# Weibull
weib1 = rGEV(100, xi = -0.5, sigma = 0.5, mu = -1)
weib2 = sort(weib1)
weib = pnorm(weib2, 0, 1)

t = (1:n)/(n + 1)

dev.new()
plot(weib, t, col = "blue", pch = 23, bg = "blue", xlab = c(""), ylab = c(""), 
    xlim = c(0, 1), ylim = c(0, 1))
lines(t, t, type = "l", col = "red", lwd = 2)
title("PP Plot of Extreme Value - Weibull")
