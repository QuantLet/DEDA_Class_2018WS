# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# set working directory
setwd("")

# install and load packages
packages = c("lmtest", "sandwich")
invisible(lapply(packages, function(pkg) {
    if (!is.element(pkg, installed.packages())) install.packages(pkg)
    library(pkg, character.only = TRUE)
}))

# read data
cpu.df = read.csv2("cpu.csv")

# new column with year as integer
cpu.df$year.int = cpu.df$Date - min(cpu.df$Date)

# regression
reg.lm  = lm(log10(cpu.df$Transistors) ~ cpu.df$year.int)

# coefficients
coeftest(reg.lm, vcov. = vcovHC)

# R^2
paste("R^2:", summary(reg.lm)$r.squared)

saveRDS(list(reg.lm = reg.lm, cpu.df = cpu.df), "cpureg.rds")
