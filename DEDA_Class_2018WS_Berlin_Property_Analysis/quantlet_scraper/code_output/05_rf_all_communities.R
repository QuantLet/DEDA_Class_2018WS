# This script runs random forest on all communities and on each community. 
# Only a dotchart of the variable importance plot from when random forest is
# ran on all communities is created. A heatmap is created from using the 
# variable importance values from all the models (using all communities and each
# community for each year). Last, a pairwise plot of the misclassification error
# rates are created.

library("randomForest")
library("plyr")
library("RColorBrewer")
library("ggplot2")

# Random forest is going to be ran many times so create function that runs
# random forest with the same dependent variable and number of tree grown.
# The value of the tuning parameter will differ for each year. Note that this
# is doing classification not regression.
Model &lt;- function(data, yVar, nVars){
  # Args:
  # data: raw data to run random forest on
  # yVar: the categorical dependent variable
  # nVars: number of variable seleted to be split on
  yVar &lt;- which(colnames(data) == yVar)
  y &lt;- as.factor(data[, yVar])
  x &lt;- data[, -yVar]
  
  set.seed(123)
  randomForest(x, 
               y,
               importance = TRUE, 
               ntree = 5000,
               mtry = nVars)
}

# GetVarImp extracts
GetVarImp &lt;- function(data, yVar, nVars){
  # Args:
  # data: raw data to run random forest on
  # yVar: the categorical dependent variable
  # nVars: number of variable seleted to be split on
  
  rf &lt;- Model(data, yVar, nVars)
  
  # Extract the random forest error rate for the misclassificaton error rate
  # pairwise plot
  error &lt;- rf$err.rate[nrow(rf$err.rate), 1]
  
  # Save the variable importance values for the dotcharts and heatmaps
  imp &lt;- sort(rf$importance[, 4], decreasing = TRUE)
  
  list(error, imp)
}

# MyRank compares all the variables that rank in the top 4 as important and
# counts the total number of times the variable shows up as the first, second,
# third, and forth most important variables.
MyRank &lt;- function(x, vars){    
  nRow &lt;- length(vars)
  ranks &lt;- rep(NA, nRow)
  
  for (i in 1:nRow) {
    ranks[i] &lt;- sum(x == vars[i])
  } 
  
  ranks
}

# GetHeatmap creates a heatmap by taking the top 4 most important variables 
# overall and from each community for each year and ranks the variables by the
# overall most frequent occuring variable to least frequent.
GetHeatmap &lt;- function(cities2008, cities2009, cities2010, 
                       overall2008, overall2009, overall2010, 
                       main, year = 2008){  
  # Args:
  # cities 2008: list of each communities variable importance values and
  #              error rate in 2008
  # cities 2009: list of each communities variable importance values and
  #              error rate in 2009
  # cities 2010: list of each communities variable importance values and
  #              error rate in 2010
  # overall 2008: the variable importance values and error rate when random
  #               forest is ran on all communities in 2008
  # overall 2009: the variable importance values and error rate when random
  #               forest is ran on all communities in 2009
  # overall 2010: the variable importance values and error rate when random
  #               forest is ran on all communities in 2010
  # title: main title for heatmap
  # year: heatmap of the year desired
  
  communities &lt;- c("OVERALL", "Aberdeen, SD", "Akron, OH", "Biloxi, MS", 
                   "Boulder, CO", "Bradenton, FL", "Charlotte, NC", 
                   "Columbia, SC", "Columbus, GA", "Detroit, MI", 
                   "Duluth, MN", "Fort Wayne, IN", "Gary, IN",         
                   "Grand Forks, ND", "Lexington, KY", "Long Beach, CA", 
                   "Macon, GA", "Miami, FL", "Milledgeville, GA", 
                   "Myrtle Beach, SC", "Palm Beach, FL", "Philadelphia, PA", 
                   "San Jose, CA", "St. Paul, MN", "State College, PA",
                   "Tallahassee, FL", "Wichita, KS")
  
  # Obtain the names of the top 4 important variables in all communities
  all2008 &lt;- names(overall2008[[2]])[1:4]
  all2009 &lt;- names(overall2009[[2]])[1:4]
  all2010 &lt;- names(overall2010[[2]])[1:4]
  
  topFour2008 &lt;- matrix(NA, nrow = 26, ncol = 4)
  topFour2009 &lt;- matrix(NA, nrow = 26, ncol = 4)
  topFour2010 &lt;- matrix(NA, nrow = 26, ncol = 4)
  
  # Obtain the names of the top 4 important variables in each community
  for (i in 1:26) {
    topFour2008[i, ] &lt;- names(cities2008[[i]][[2]])[1:4]
    topFour2009[i, ] &lt;- names(cities2009[[i]][[2]])[1:4]
    topFour2010[i, ] &lt;- names(cities2010[[i]][[2]])[1:4]
  }
  
  topFour2008 &lt;- rbind(topFour2008, all2008)
  topFour2009 &lt;- rbind(topFour2009, all2009)
  topFour2010 &lt;- rbind(topFour2010, all2009)
  
  # Combine all the important variable across three year so we can calculate
  # the most frequent variables
  top4 &lt;- rbind(topFour2008, topFour2009, topFour2010)
  
  # Obtain the number of times each variable is in the top 4
  vars &lt;- names(table(top4)[order(table(top4))])
  
  # Multiply the number of times a variable was ranked as the first most
  # important variable by 4, the second by 3, the third by 2, and the fourth
  # by 1. Sum across rows gives a value that represents how important that 
  # variable or where that variable ranks compared to the others. 
  myOrder &lt;- apply(top4, 2, MyRank, vars) %*% 4:1
  
  # Obtain the variable rank in order to determine the order of the columns of 
  # the heatmap
  myOrder &lt;- vars[order(myOrder, decreasing = TRUE)]
  
  error08 &lt;- rep(NA, 27)
  error09 &lt;- error08
  error10 &lt;- error08
  
  # Extract the error rate when all communities are fitted in random forests
  error08[1] &lt;- overall2008[[1]]
  error09[1] &lt;- overall2009[[1]]
  error10[1] &lt;- overall2010[[1]]
  
  # Extract the error rate of each community
  for (i in 1:26) {
    error08[i + 1] &lt;- cities2008[[i]][[1]]
    error09[i + 1] &lt;- cities2009[[i]][[1]]
    error10[i + 1] &lt;- cities2010[[i]][[1]]
  }
  
  error08 &lt;- data.frame(communities, error = error08)[length(communities):1, ]
  error09 &lt;- data.frame(communities, error = error09)[length(communities):1, ]
  error10 &lt;- data.frame(communities, error = error10)[length(communities):1, ]
  
  error08 &lt;- error08[nrow(error08):1, ]
  error09 &lt;- error09[nrow(error09):1, ]
  error10 &lt;- error10[nrow(error10):1, ]
  
  # Calculate the x-axis range for the dotchart of the error rates
  minError &lt;- min(c(error08$error, error09$error, error10$error))
  maxError &lt;- max(c(error08$error, error09$error, error10$error))
  
  minError &lt;- .15
  
  # Order the communities for each year by the 2008 data set error rates
  orderByError &lt;- order(error08$error)
  
  # Which data should be used to create the heatmap and dotchart?
  if (year == 2008) {
    cities &lt;- cities2008
    overall &lt;- overall2008[[2]]
    error &lt;- error08
  } else if (year == 2009) {
    cities &lt;- cities2009
    overall &lt;- overall2009[[2]]
    error &lt;- error09
  } else {
    cities &lt;- cities2010
    overall &lt;- overall2010[[2]]
    error &lt;- error10
  }
  
  # The number of columns in the heatmap are the number of unique variables
  # that are the top 4 most important variables out of all the models ran.
  nVars &lt;- length(vars)
  rankedVars &lt;- matrix(NA, nrow = length(cities), ncol = nVars)
  
  all &lt;- names(overall[1:nVars])
  
  for (i in 1:length(cities)) {
    rankedVars[i, ] &lt;- names(cities[[i]][[2]])[1:nVars]
  }
  
  rankedVars &lt;- rbind(all, rankedVars)
  
  # The rows of the heatmap will represent the communities
  rownames(rankedVars) &lt;- communities
  
  # Use numerical values to represent the most important variable for a 
  # community in a heatmap matrix from 1 to 4, 1 being the most important in 
  # that model, 2 the second most important, etc., so that we can use the image 
  # function to plot the heatmap.
  heatmap &lt;- matrix(0, nrow = nrow(rankedVars), ncol = nVars)
  for (i in 1:nrow(rankedVars)) {
    heatmap[i, which(rankedVars[i, 1] == myOrder)] &lt;- 1
    heatmap[i, which(rankedVars[i, 2] == myOrder)] &lt;- 2
    heatmap[i, which(rankedVars[i, 3] == myOrder)] &lt;- 3
    heatmap[i, which(rankedVars[i, 4] == myOrder)] &lt;- 4
  }
  
  rownames(heatmap) &lt;- communities
  
  # Order the error rates by the 2008 error rates such that it appears larget 
  # to smallest on the dotchart
  error &lt;- error[orderByError, ]
  error$communities &lt;- factor(error$communities, 
                              levels = error$communities[nrow(error):1])
  error &lt;- error[nrow(error):1, ]
  
  # order the heatmap according to the 2008 misclassification error rates
  heatmap &lt;- heatmap[orderByError, ]
  
  # Make room for the dotcharts on the right of the heatmaps
  layout(matrix(c(1, 2), 1, 2, byrow = TRUE), width = c(7.5, 2.5))
  
  # Plot the first 12 columns of the heatmap since that's where most of the 
  # action is
  par(xpd = TRUE, mar = c(4, 10, 3, 7))
  colors &lt;- brewer.pal(5, "OrRd")[c(1, 5:2)]
  image(t(heatmap)[1:12, nrow(heatmap):1], 
        col = colors,
        axes = FALSE,
        main = main,
        xlab = "Important Predictor Variables",
        cex.main = 1.5,
        cex.lab = 1.5)
  axis(1, 
       at = seq(0, 1, length.out = 12), 
       labels = myOrder[1:12], 
       cex.axis = 1.2, 
       tick = TRUE, 
       las = 1)
  axis(2, 
       at = seq(0, 1, length.out = nrow(heatmap)), 
       labels = error$communities, 
       cex.axis = 1,
       tick = TRUE, 
       las = 1)
  mtext("Communities", 
        side = 2, 
        outer = TRUE, 
        line = -2.5,
        adj = 0.55,
        cex = 1.5)
  legend(1.05, 1,
         col = colors,
         pch = 15,
         legend = c("None", "Rank 1", "Rank 2", "Rank 3", "Rank 4"),
         cex = 1.2)
  
  par(mar = c(4, 0, 3, 1))
  dotchart(error$error, 
           xlim = c(minError, maxError),
           pch = 16,
           xlab = "Misclassification Error Rate",
           cex.lab = 1.5)
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

cities &lt;- c("Aberdeen, SD",      "Akron, OH",         "Biloxi, MS",       
            "Boulder, CO",       "Bradenton, FL",     "Charlotte, NC",    
            "Columbia, SC",      "Columbus, GA",      "Detroit, MI",      
            "Duluth, MN",        "Fort Wayne, IN",    "Gary, IN",         
            "Grand Forks, ND",   "Lexington, KY",     "Long Beach, CA",   
            "Macon, GA",         "Miami, FL",         "Milledgeville, GA",
            "Myrtle Beach, SC",  "Palm Beach, FL",    "Philadelphia, PA", 
            "San Jose, CA",      "St. Paul, MN",      "State College, PA",
            "Tallahassee, FL",   "Wichita, KS")

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

# Run random forests on each community and obtain a list of variable
# importance values and misclassification error rates
cities2008 &lt;- dlply(data1, "qsb", GetVarImp, "ccegrp", 6)
cities2009 &lt;- dlply(data2, "qsb", GetVarImp, "ccegrp", 7)
cities2010 &lt;- dlply(data3, "qsb", GetVarImp, "ccegrp", 4)

# Run random forest on all communities and obtain the variable importance 
# values and misclassification error rates
overall2008 &lt;- GetVarImp(data1, "ccegrp", 6)
overall2009 &lt;- GetVarImp(data2, "ccegrp", 7)
overall2010 &lt;- GetVarImp(data3, "ccegrp", 4)

symbols &lt;- c(16, 17, 15)
colors &lt;- brewer.pal(3, "Set2")

# Extract importance values and variable names to create a variable importance
# plot for all three years
data2008 &lt;- data.frame(vars = names(overall2008[[2]])[1:12], 
                       val = as.vector(overall2008[[2]])[1:12])
data2009 &lt;- data.frame(vars = names(overall2009[[2]])[1:12], 
                       val = as.vector(overall2009[[2]])[1:12])
data2010 &lt;- data.frame(vars = names(overall2010[[2]])[1:12], 
                       val = as.vector(overall2010[[2]])[1:12])

colnames(data2008) &lt;- c("vars", "val")
colnames(data2009) &lt;- c("vars", "val")
colnames(data2010) &lt;- c("vars", "val")

# Order data in such a way that the most important variables are listed at the
# top of the dotchart when using the ggplot function
data2008 &lt;- data2008[12:1, ]
data2009 &lt;- data2009[12:1, ]
data2010 &lt;- data2010[12:1, ]

data2008$vars &lt;- factor(data2008$vars, levels = data2008$vars)
data2009$vars &lt;- factor(data2009$vars, levels = data2009$vars)
data2010$vars &lt;- factor(data2010$vars, levels = data2010$vars)

####################################################################
# Create a dotchart the variable importance for each year
####################################################################
png("rf2008.png",
    height = 400,
    width = 400)
print(ggplot(data2008, aes(x = val, y = vars)) +
      geom_point(size = 5, shape = symbols[1], color = colors[1]) +
      labs(title = "Random Forests Variable Importance Plot - Year 2008",
           x = "Mean Decrease in Accuracy",
           y = "Predictor Variables") +
      scale_x_continuous(limits = c(0, 0.05)) +
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18),
            title = element_text(size = 11)))
dev.off()

png("rf2009.png",
    height = 400,
    width = 400)
print(ggplot(data2009, aes(x = val, y = vars)) +
      geom_point(size = 5, shape = symbols[2], color = colors[2]) +
      labs(title = "Random Forests Variable Importance Plot - Year 2009",
           x = "Mean Decrease in Accuracy",
           y = "Predictor Variables") +
      scale_x_continuous(limits = c(0, 0.05)) +
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18),
            title = element_text(size = 11)))
dev.off()

png("rf2010.png",
    height = 400,
    width = 400)
print(ggplot(data2010, aes(x = val, y = vars)) +
      geom_point(size = 5, shape = symbols[3], color = colors[3]) +
      labs(title = "Random Forests Variable Importance Plot - Year 2010",
           x = "Mean Decrease in Accuracy",
           y = "Predictor Variables") +
      scale_x_continuous(limits = c(0, 0.05)) +
      theme(panel.background = element_blank(), 
            panel.grid.major.y = element_line(colour = "grey"),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 18),
            title = element_text(size = 11)))
dev.off()

#####################################################################
# Create the heatmaps for each year
#####################################################################

png("heatmap2008.png", 
    width = 1100,
    height = 550)
print(GetHeatmap(cities2008, cities2009, cities2010, 
                 overall2008, overall2009, overall2010, 
                 main = "Predicting Attachment in the Year of 2008",
                 year = 2008))
dev.off()

png("heatmap2009.png", 
    width = 1100,
    height = 550)
print(GetHeatmap(cities2008, cities2009, cities2010, 
                 overall2008, overall2009, overall2010, 
                 main = "Predicting Attachment in the Year of 2009",
                 year = 2009))
dev.off()

png("heatmap2010.png", 
    width = 1100,
    height = 550)
print(GetHeatmap(cities2008, cities2009, cities2010, 
                 overall2008, overall2009, overall2010, 
                 main = "Predicting Attachment in the Year of 2010",
                 year = 2010))
dev.off()

#####################################################################
# Pairwise scatter plot of misclassification errors
#####################################################################

# Create a vector of the random forests model misclassification error 
# rates for each year to make a pairwise scatterplot
error08 &lt;- rep(NA, 27)
error09 &lt;- error08
error10 &lt;- error08

# Obtain the error rate for each year when random forest is ran on all
# communities
error08[1] &lt;- overall2008[[1]]
error09[1] &lt;- overall2009[[1]]
error10[1] &lt;- overall2010[[1]]

# Obtain the error rate for each year when random forest is ran on each
# communities
for (i in 1:26) {
  error08[i + 1] &lt;- cities2008[[i]][[1]]
  error09[i + 1] &lt;- cities2009[[i]][[1]]
  error10[i + 1] &lt;- cities2010[[i]][[1]]
}

# Is the level of difficulty in classifying attachment status the same 
# each year?
cor(error08, error09)
cor(error08, error10)
cor(error09, error10)

png("PairsPlot_MisclassificationError.png")
print(pairs(cbind(error08, error09, error10),
            labels = c("Misclassification\nError Rate in 2008", 
                       "Misclassification\nError Rate in 2009", 
                       "Misclassification\nError Rate in 2010"),
            pch = 16,
            cex.labels = 1.5,
            cex.main = 2,
            cex.lab = 1.2,
            cex.axis = 1.1))
dev.off()
