# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
libraries = c("POT")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

# load data
a = read.table("BAYER_close_0012.dat")
b = read.table("BMW_close_0012.dat")
c = read.table("SIEMENS_close_0012.dat")
e = read.table("VW_close_0012.dat")

# Portfolio
d = a + b + c + e

n1 = dim(d)[1]  # length of portfolio
x = log(d[1:n1 - 1, ]/d[2:n1, ])  # negative log-returns

gpd = fitgpd(x, quantile(x, 0.95), est = "mle")  # 
n = gpd$nat
thr = gpd$threshold
scale = gpd$param[1]
shape = gpd$param[2]
data = gpd$data
exc = gpd$exceed
t = (1:n)/(n + 1)
y1 = qgpd(t, scale = scale, shape = shape)

gpdt = sort(exc) - thr
y2 = pgpd(gpdt, scale = scale, shape = shape)

# plot
plot(gpdt, y1, col = "blue", pch = 15, bg = "blue", xlab = "", ylab = "", main = "QQ plot, Generalized Pareto Distribution")
lines(y1, y1, type = "l", col = "red", lwd = 2)
