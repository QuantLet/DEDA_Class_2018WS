# This script runs random forests only on Gary, Indiana. It then plots a 
# dotchart of the variable importance from random forests then creates
# a parallel coordinate plot of the top 12 raw important variables for each
# of the three years.

library(RColorBrewer)
library(GGally)
library(randomForest)
library(plyr)
library(ggplot2)

# GetPCP takes the raw data and the variables interested in to be plotted in 
# the parallel coordinate plot
GetPCP &lt;- function(data, myvars, mylevels, mycolors, mytitle){
  # Args:
  # data: raw data set containing myvars
  # myvars: names of variables used in parallel coordinate plot
  # mylevels: name of the dependents variable levels
  # mycolors: choice of colors for dependent variable levels
  # mytitle: main title of parallel coordinate plot
  
  y &lt;- data$ccegrp 
  x &lt;- data[, myvars]
  
  # Variables interested are all categorical so we need to jitter the data
  set.seed(123)
  x &lt;- apply(x, 2, jitter)
  
  # Rearrange data for convenience
  pcpData &lt;- data.frame(x, y = y)
  pcpData$y &lt;- factor(pcpData$y, levels = mylevels)
  
  colors &lt;- mycolors
  
  p &lt;- ggparcoord(pcpData, 
                  columns = 1:(ncol(pcpData) - 1), 
                  groupColumn = ncol(pcpData), 
                  scale = "globalminmax",
                  alphaLines = 0.5, 
                  title = mytitle)
  
  p + 
  geom_line(aes(colour = y)) +
  scale_colour_manual(values = mycolors) +
  labs(x = "Variables",
       y = "Values",
       colour = "Attached?") +
  theme(plot.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
}

data1 &lt;- read.csv("FinalData08.csv")
data2 &lt;- read.csv("FinalData09.csv")
data3 &lt;- read.csv("FinalData10.csv")

data1 &lt;- na.omit(data1)
data2 &lt;- na.omit(data2)
data3 &lt;- na.omit(data3)

data1 &lt;- data1[order(data1$ccegrp), ]
data2 &lt;- data2[order(data2$ccegrp), ]
data3 &lt;- data3[order(data3$ccegrp), ]

data1$ccegrp &lt;- mapvalues(data1$ccegrp, 1:3, 
                          c("Not Attached", "Neutral", "Attached"))
data2$ccegrp &lt;- mapvalues(data2$ccegrp, 1:3, 
                          c("Not Attached", "Neutral", "Attached"))
data3$ccegrp &lt;- mapvalues(data3$ccegrp, 1:3, 
                          c("Not Attached", "Neutral", "Attached"))

# Use this data set from the parallel coordinate plot so we can jitter
# the categorical variables
gary08 &lt;- data1[data1$qsb == 12, ]
gary09 &lt;- data2[data2$qsb == 12, ]
gary10 &lt;- data3[data3$qsb == 12, ]

catVars &lt;- c("qsb",      "qs4",      "q3c",      "q5",       "q6",      
             "q7a",      "q7b",      "q7c",      "q7d",      "q7e",     
             "q7f",      "q7g",      "q7h",      "q7i",      "q7k",     
             "q7l",      "q7m",      "q8a",      "q8b",      "q8c",     
             "q8d",      "q8e",      "q8f",      "q9",       "q10",     
             "q11",      "q15aa",    "q15ab",    "q18",      "q19",     
             "q20",      "q21",      "q22a",     "q22b",     "q22c",    
             "q22d",     "q23",      "q24",      "q25",      "q26",     
             "qd4",      "qd6",      "qd7",      "qd8",      "qd9",     
             "qd10",     "qd111",    "qd13",     "urban_gr", "city",    
             "ccegrp")

for (i in 1:length(catVars)) {
  data1[, catVars[i]] &lt;- factor(data1[, catVars[i]])
  data2[, catVars[i]] &lt;- factor(data2[, catVars[i]])
  data3[, catVars[i]] &lt;- factor(data3[, catVars[i]])
}

# Use this data set in random forests since the dependent variable is now
# a factor variable
gary2008 &lt;- data1[data1$qsb == 12, ]
gary2009 &lt;- data2[data2$qsb == 12, ]
gary2010 &lt;- data3[data3$qsb == 12, ]

# Run random forests using the mtrys determined from cross validation in the
# tuning parameters script and use the number of trees where the variable 
# importance is stable and the misclassification error rate is constant and 
# unchanging
set.seed(123)
rf08 &lt;- randomForest(gary2008[, 1:54], 
                     gary2008[, 55],
                     importance = TRUE, 
                     ntree = 5000,
                     mtry = 6)
set.seed(123)
rf09 &lt;- randomForest(gary2009[, 1:54], 
                     gary2009[, 55],
                     importance = TRUE, 
                     ntree = 5000,
                     mtry = 7)
set.seed(123)
rf10 &lt;- randomForest(gary2010[, 1:54], 
                     gary2010[, 55],
                     importance = TRUE, 
                     ntree = 5000,
                     mtry = 4)

symbols &lt;- c(16, 17, 15)
colors &lt;- brewer.pal(3, "Set2")

# Order the variables so it shows the most important variables at top and 
# least important towards the bottom of the dotchart
varImp2008 &lt;- sort(rf08$importance[, 4])[(ncol(data1) - 12):(ncol(data1) - 1)]
varImp2009 &lt;- sort(rf09$importance[, 4])[(ncol(data1) - 12):(ncol(data1) - 1)]
varImp2010 &lt;- sort(rf10$importance[, 4])[(ncol(data1) - 12):(ncol(data1) - 1)]

# Get the data set right for the ggplot function
varImp2008 &lt;- data.frame(vars = names(varImp2008), val = as.numeric(varImp2008))
varImp2009 &lt;- data.frame(vars = names(varImp2009), val = as.numeric(varImp2009))
varImp2010 &lt;- data.frame(vars = names(varImp2010), val = as.numeric(varImp2010))

varImp2008$vars &lt;- factor(varImp2008$vars, levels = varImp2008$vars)
varImp2009$vars &lt;- factor(varImp2009$vars, levels = varImp2009$vars)
varImp2010$vars &lt;- factor(varImp2010$vars, levels = varImp2010$vars)

mytitle1 &lt;- "Random Forests Variable Importance"
mytitle2 &lt;- "Plot for Gary, Indiana\nYear 2008"
png("gary2008VIplot.png",
    height = 400,
    width = 400)
print(ggplot(varImp2008, aes(x = val, y = vars)) +
      geom_point(size = 5, shape = symbols[1], color = colors[1]) +
      labs(title = paste(mytitle1, mytitle2, sep = " "),
           x = "Mean Decrease in Accuracy",
           y = "Predictor Variables") +
      scale_x_continuous(limits = c(0, 0.04)) +
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18),
            title = element_text(size = 11)))
dev.off()

mytitle1 &lt;- "Random Forests Variable Importance"
mytitle2 &lt;- "Plot for Gary, Indiana\nYear 2009"
png("gary2009VIplot.png",
    height = 400,
    width = 400)
print(ggplot(varImp2009, aes(x = val, y = vars)) +
      geom_point(size = 5, shape = symbols[2], color = colors[2]) +
      labs(title = paste(mytitle1, mytitle2, sep = " "),
           x = "Mean Decrease in Accuracy",
           y = "Predictor Variables") +
      scale_x_continuous(limits = c(0, 0.04)) +
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18),
            title = element_text(size = 11)))
dev.off()

mytitle1 &lt;- "Random Forests Variable Importance"
mytitle2 &lt;- "Plot for Gary, Indiana\nYear 2010"
png("gary2010VIplot.png",
    height = 400,
    width = 400)
print(ggplot(varImp2010, aes(x = val, y = vars)) +
      geom_point(size = 5, shape = symbols[3], color = colors[3]) +
      labs(title = paste(mytitle1, mytitle2, sep = " "),
           x = "Mean Decrease in Accuracy",
           y = "Predictor Variables") +
      scale_x_continuous(limits = c(0, 0.04)) +
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18),
            title = element_text(size = 11)))
dev.off()

# Order the variables in the parallel coordinate plot from most important
# to least important according to random forests
var08 &lt;- names(sort(rf08$importance[, 4])[(ncol(data1) - 1):(ncol(data1) - 12)])
var09 &lt;- names(sort(rf09$importance[, 4])[(ncol(data1) - 1):(ncol(data1) - 12)])
var10 &lt;- names(sort(rf10$importance[, 4])[(ncol(data1) - 1):(ncol(data1) - 12)])

colors &lt;- brewer.pal(4, "PuOr")[c(1, 3, 4)]

png("Gary08pcp.png",
    height = 310,
    width =  686)
print(GetPCP(data = gary08, 
             myvars = var08, 
             mylevels = c("Attached", "Neutral", "Not Attached"), 
             mycolors = colors,
             mytitle = "Gary, Indiana, in 2008"))
dev.off()

png("Gary09pcp.png",
    height = 310,
    width =  686)
print(GetPCP(data = gary09, 
             myvars = var09, 
             mylevels = c("Attached", "Neutral", "Not Attached"), 
             mycolors = colors,
             mytitle = "Gary, Indiana, in 2009"))
dev.off()

png("Gary10pcp.png",
    height = 310,
    width =  686)
print(GetPCP(data = gary10, 
             myvars = var10, 
             mylevels = c("Attached", "Neutral", "Not Attached"), 
             mycolors = colors,
             mytitle = "Gary, Indiana, in 2010"))
dev.off()
