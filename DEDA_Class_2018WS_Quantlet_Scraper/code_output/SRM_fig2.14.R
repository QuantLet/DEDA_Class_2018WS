## clear history
rm(list = ls(all = TRUE))
graphics.off()

## Q-Q-Normal-Plots mit Sollgeraden
## Mit festem seed zur Reproduktion
set.seed(1234)
w = rnorm(100)
set.seed(1234)
x = rnorm(100, mean = 5)
set.seed(1234)
y = rnorm(100, sd = 3)
set.seed(1234)
z = rnorm(100, mean = 5, sd = 3)

par(mfrow = c(2, 2))
qqnorm(w, main = "Normal Q-Q-Plot der Stichprobe w", xlab = "theoretische Standardnormal-Quantile", 
       ylab = "empirische Quantile")
qqline(w)

qqnorm(x, main = "Normal Q-Q-Plot der Stichprobe x", xlab = "theoretische Standardnormal-Quantile", 
       ylab = "empirische Quantile")
qqline(x)

qqnorm(y, main = "Normal Q-Q-Plot der Stichprobe y", xlab = "theoretische Standardnormal-Quantile", 
       ylab = "empirische Quantile")
qqline(y)

qqnorm(z, main = "Normal Q-Q-Plot der Stichprobe z", xlab = "theoretische Standardnormal-Quantile", 
       ylab = "empirische Quantile")
qqline(z)
