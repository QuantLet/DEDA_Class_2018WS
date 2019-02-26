# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# set working directory
#setwd("")
options(stringsAsFactors = FALSE)

# install and load packages
packages = c("sandwich", "lmtest", "zoo")
invisible(lapply(packages, function(pkg) {
    if (!is.element(pkg, installed.packages())) install.packages(pkg)
    library(pkg, character.only = TRUE)
}))

# read data
hdd.df = read.csv2("hdd.csv")

# date formatting
# (day mostly missing, not important for further analysis, therefore dropped)
hdd.df$date = as.Date(as.yearmon(hdd.df$date, "%Y %B"))

# create month vector
months     = seq(min(hdd.df$date), max(hdd.df$date), "months")
months.int = seq(1, length(months))
month.df   = data.frame(date = months, int = months.int)

# merge dfs
hdd.df = merge(hdd.df, month.df, by = "date")

# regression
reg.lm  = lm(log(hdd.df$per.GB) ~ hdd.df$int)

# coefficients
coeftest(reg.lm, vcov. = vcovHC)

# R^2
paste("R^2:", summary(reg.lm)$r.squared)
