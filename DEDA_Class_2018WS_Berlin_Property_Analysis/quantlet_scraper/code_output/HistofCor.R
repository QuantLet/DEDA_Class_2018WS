# install and load packages
libraries = c("multilevel", "data.table")
lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

data(bh1996) # data set from package "multilevel" to exemplify the function

# input into function: 
   # cluster variable: GRP
   # 1. level 1 variable: COHES
   # 2. level 1 variable: LEAD

data    = bh1996[c("GRP", "COHES", "LEAD")] #extract relevant variables from data frame
var1    = "COHES"
var2    = "LEAD"
cluster = "GRP"

# 1. level_2 variable (i. e. means) is calculated for both level1 variables per cluster 
# 2. correlation on level_2 is computed
# 3. one correlation of both variables on level_1 is computed per cluster
# 4. plot of the results

HistOfCor = function(data, var1, var2, cluster) {
  variable.names                        =  c(var1, var2, cluster)  # rename to get variables
  names(data)[names(data) == var1]      = "lvl1.var1"
  names(data)[names(data) == var2]      = "lvl1.var2"
  names(data)[names(data) == cluster]   = "cluster"
  dt = data.table(data)  # transform data frame to data table
    
  # 1. compute means (type of correlation can be changed!) and within correlation
  level1.values.dt = dt[,list(mean1 = mean(lvl1.var1, na.rm = T),
                              mean2 = mean(lvl1.var2, na.rm = T),
                              cor.lvl1 = cor(lvl1.var1, lvl1.var2, use = "complete.obs")), 
                              by = cluster]
      
  # 2. level 2 correlation of means (i. e. of variables on level 2)
  cor.lvll2   = with(level1.values.dt, cor.test(mean1, mean2, use = "complete.obs"))
  lvl2.cor.r  = round(cor.lvll2$estimate, 3)
  lvl2.cor.p  = round(cor.lvll2$p.value, 3)
  lvl2.cor.df = round(cor.lvll2$parameter, 3)
  
  # 3. level 1 correlation across clusters
  cor.lvll1   = with(dt, cor.test(lvl1.var1, lvl1.var2, use = "complete.obs"))
  lvl1.cor.r  = round(cor.lvll1$estimate, 3)
  lvl1.cor.p  = round(cor.lvll1$p.value, 3)
  lvl1.cor.df = round(cor.lvll1$parameter, 3)
   
  # 4. plot 
  plot.new()
  hist(level1.values.dt$cor.lvl1, breaks = 20, xlim = c(-1, 1),  # takes correlations from above
        main="", xlab="", ylab="")  # main = variable name
  
  head.text = bquote(paste("Correlation of ", bold(.(var1)), " and ", bold(.(var2)), sep = " "))  
  mtext(side = 3, head.text, line = 2, cex = .8)  # variable for which the correlations are produced
  abline(v = lvl2.cor.r, col = "red")  # add line lvl2 cor 
  abline(v = lvl1.cor.r, col = "forestgreen")  # add line lvl1 cor (correlation across clusters)
  meanofcor = mean(level1.values.dt$cor.lvl1, na.rm=T)  
  abline(v = meanofcor, col = "blue") # add line lvl1 cor mean
  testval.2 = bquote(paste("Level-2 correlartion (means, red): ", italic("r "), "= ", .(lvl2.cor.r), 
                           ", ", italic("p"), " = ",  .(lvl2.cor.p), ", with ", italic("df"), " = ", 
                           .(lvl2.cor.df),  sep = ""))  # add test stat summary
  mtext(side = 3, testval.2, line = 0, cex = .6, at = 0)
  testval.1 = bquote(paste("Level-1 correlation (no cluster structure, green): ", italic("r "), "= ", 
                           .(lvl1.cor.r), ", ", italic("p"), " = ",  .(lvl1.cor.p), ", with ", 
                           italic("df"), " = ", .(lvl1.cor.df),  sep = ""))  
  mtext(side = 3, testval.1, line = 1, cex = .6, at = 0)
   
  out.data = as.data.frame(level1.values.dt)
  name1    = paste("mean", var1, sep = "")
  name2    = paste("mean", var2, sep = "")
  name.cor = "corOnLvl1"
  colnames(out.data) = c(cluster, name1, name2, name.cor)
  results = list(out.data, cor.lvll2)
  results
}

# get output
# quartz()
(result = HistOfCor(bh1996, var1 = "COHES", var2 = "LEAD", cluster = "GRP"))
