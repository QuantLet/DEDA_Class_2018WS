# clear variables and close windows
rm(list = ls(all = TRUE))
graphics.off()

# install and load packages
if (!require("fGarch")) {
    install.packages("fGarch")
}
library(fGarch)
if (!require("grid")) {
    install.packages("grid")
}
library(grid)
if (!require("gridExtra")) {
    install.packages("gridExtra")
}
library(gridExtra)
if (!require("gridGraphics")) {
  install.packages("gridGraphics")
}
library(gridGraphics)

#set seed
set.seed(134)

#set n for n=1000
sim1000 = garchSim(spec = garchSpec(model = list(alpha = 0.9, beta = 0)),1000)
#plot PACF of squared Observations
sim10002 = sim1000^2
pacf(sim10002)
#ARCH model estimation
model1000  =  garchFit(~garch(1), data =sim1000,cond.dist = "QMLE")
#plot of Time Series
plot(sim1000@.Data,type="l",xlab="", ylab="",ylim=c(-0.02,0.02))
#Quantile-Quantile Plot of Standardized Residuals
res=model1000@residuals
sig=(model1000@sigma.t)^2
std_res=res/sig/1000
length(std_res)
qqplot(qnorm((1:1000-0.5)/1000),sort(std_res),ylim=c(-3,3),xlim=c(-3,3),xlab="Theoretical Quantiles",ylab="Sample Quantiles")
abline(mean(std_res), sd(std_res), col="blue", lwd=2)

# Simulation: function takes an input for # replications =k 
#(by default it is 1000) and seed (by default it is 123)
set.seed(123) #arbitrary seed
simulation = function(size_of_dataset, k = 1000) {
    list_alpha = c()
    h          = c()
    diff       = c()
    
    for (i in 1:k) {
        modelArch  = garchFit(~garch(1), data = garchSim(spec = garchSpec(model = list(alpha = 0.9, 
            beta = 0)), n = size_of_dataset), cond.dist = "QMLE")
        list_alpha = c(list_alpha, coef(modelArch)[3])
    }
    for (alpha in list_alpha) {
        tmp  = (alpha - 0.9)^2
        diff = c(diff, tmp)
    }
    
    count = 0
    for (alpha in list_alpha) {
        if (round(alpha, 7) &gt;= 1) {
            count = count + 1
        }
    }
    results = c(mean = mean(list_alpha), sd = (sum(diff)/length(diff))^0.5, alpha_bigger_or_equal_1 = count/length(list_alpha))
    return(results)
}


Dataset100  = simulation(100)
Dataset250  = simulation(250)
Dataset500  = simulation(500)
Dataset1000 = simulation(1000)

# Constructing a table which sums up the results
rowNames       = c("Dataset100", "Dataset250", "Dataset500", "Dataset1000")
columnNames    = c("average of alpha", "sd of alpha", "alpha &gt;= 1")
data           = matrix(NA, length(rowNames), length(columnNames))
rownames(data) = rowNames
colnames(data) = columnNames
data[, ]       = matrix(1:12, 3, 4)
data[1, 1:3]   = Dataset100[1:3]
data[2, 1:3]   = Dataset250[1:3]
data[3, 1:3]   = Dataset500[1:3]
data[4, 1:3]   = Dataset1000[1:3]
png(filename   = "table of results.png")
grid.table(round(data,3))
dev.off()

# for Knuth Knuth-TAOCP-2002 RNG
RNGkind(kind = "Knuth-TAOCP-2002", normal.kind = NULL)
Dataset100Knuth        = simulation(100)
Dataset250Knuth        = simulation(250)
Dataset500Knuth        = simulation(500)
Dataset1000Knuth       = simulation(1000)
dataRNGKnuth           = matrix(NA, length(rowNames), length(columnNames))
rownames(dataRNGKnuth) = rowNames
colnames(dataRNGKnuth) = columnNames
dataRNGKnuth[, ]       = matrix(1:12, 3, 4)
dataRNGKnuth[1, 1:3]   = Dataset100Knuth[1:3]
dataRNGKnuth[2, 1:3]   = Dataset250Knuth[1:3]
dataRNGKnuth[3, 1:3]   = Dataset500Knuth[1:3]
dataRNGKnuth[4, 1:3]   = Dataset1000Knuth[1:3]
png(filename = "Results for Knuth 2002.png")
grid.table(round(dataRNGKnuth,3))
dev.off()

# for Wichmann-Hill RNG
RNGkind(kind = "Wichmann-Hill", normal.kind = NULL)
Dataset100WH         = simulation(100)
Dataset250WH         = simulation(250)
Dataset500WH         = simulation(500)
Dataset1000WH        = simulation(1000)
dataRNGWH            = matrix(NA, length(rowNames), length(columnNames))
rownames(dataRNGWH)  = rowNames
colnames(dataRNGWH)  = columnNames
dataRNGWH[, ]        = matrix(1:12, 3, 4)
dataRNGWH[1, 1:3]    = Dataset100WH[1:3]
dataRNGWH[2, 1:3]    = Dataset250WH[1:3]
dataRNGWH[3, 1:3]    = Dataset500WH[1:3]
dataRNGWH[4, 1:3]    = Dataset1000WH[1:3]
png(filename = "Results for Wichmann Hill.png")
grid.table(round(dataRNGWH,3))
dev.off()

# back to default setting for RNG
RNGkind(kind = "default", normal.kind = "default")
