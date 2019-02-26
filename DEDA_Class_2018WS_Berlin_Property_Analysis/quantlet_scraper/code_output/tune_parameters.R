# This script is tuning the parameter(s) for random forests, SVM, and RPART and
# calculating the misclassification error rates.

library(caret)
library(e1071)

data1 &lt;- read.csv("FinalData08.csv")
data2 &lt;- read.csv("FinalData09.csv")
data3 &lt;- read.csv("FinalData10.csv")

data1 &lt;- na.omit(data1)
data2 &lt;- na.omit(data2)
data3 &lt;- na.omit(data3)

data1 &lt;- data1[order(data1$ccegrp), ]
data2 &lt;- data2[order(data2$ccegrp), ]
data3 &lt;- data3[order(data3$ccegrp), ]

yPos &lt;- which(colnames(data1) == "ccegrp")

# Rearrange data and rename the dependent variable for convenience
data1 &lt;- cbind(y = data1[, yPos], data1[, -yPos])
data2 &lt;- cbind(y = data2[, yPos], data2[, -yPos])
data3 &lt;- cbind(y = data3[, yPos], data3[, -yPos])

# Randomly select 3000 values (cases to be part of the training set)
set.seed(111)
inTrain08 &lt;- sample(1:nrow(data1), 3000)
set.seed(111)
inTrain09 &lt;- sample(1:nrow(data2), 3000)
set.seed(111)
inTrain10 &lt;- sample(1:nrow(data3), 3000)

# Subset the data sets into training and test set so that we can use the train 
# function from the caret package to do cross validation and to calculate
# the misclassification error rates
train08 &lt;- data1[inTrain08, ]
train09 &lt;- data2[inTrain09, ]
train10 &lt;- data3[inTrain10, ]

test08 &lt;- data1[-inTrain08, ]
test09 &lt;- data2[-inTrain09, ]
test10 &lt;- data3[-inTrain10, ]

trainX08 &lt;- train08[, -1]
trainX09 &lt;- train09[, -1]
trainX10 &lt;- train10[, -1]

trainY08 &lt;- as.factor(train08[, 1])
trainY09 &lt;- as.factor(train09[, 1])
trainY10 &lt;- as.factor(train10[, 1])

testX08 &lt;- test08[, -1]
testX09 &lt;- test09[, -1]
testX10 &lt;- test10[, -1]

# Do 10-fold crossvalidataion
fitControl08 &lt;- trainControl(method = "cv", number = 10)
fitControl09 &lt;- trainControl(method = "cv", number = 10)
fitControl10 &lt;- trainControl(method = "cv", number = 10)

########################################################################
# rpart
########################################################################
# I looked at a wide range of values for cp, but I'm only looking
# at the best couple of values to make the code run faster for other 
# users
rpartGrid08 &lt;- expand.grid(.cp = seq(0, .03, by = .01)) 
rpartFit08 &lt;- train(trainX08, 
                    trainY08, 
                    "rpart", 
                    tuneGrid = rpartGrid08, 
                    trControl = fitControl08)
rpartFit08
yhat08 &lt;- predict(rpartFit08, newdata = testX08)
rpartMse &lt;- mean(yhat08 != test08$y)
rpartMse

rpartGrid09 &lt;- expand.grid(.cp = seq(0, .03, by = .01)) 
rpartFit09 &lt;- train(trainX09, 
                    trainY09, 
                    "rpart", 
                    tuneGrid = rpartGrid09, 
                    trControl = fitControl09)
rpartFit09
yhat09 &lt;- predict(rpartFit09, newdata = testX09)
rpartMse09 &lt;- mean(yhat09 != test09$y)
rpartMse09

rpartGrid10 &lt;- expand.grid(.cp = seq(0, .03, by = .01)) 
rpartFit10 &lt;- train(trainX10, 
                    trainY10, 
                    "rpart", 
                    tuneGrid = rpartGrid10, 
                    trControl = fitControl10)
rpartFit10
yhat10 &lt;- predict(rpartFit10, newdata = testX10)
rpartMse10 &lt;- mean(yhat10 != test10$y)
rpartMse10

########################################################################
# Random Forest (RF)
########################################################################
# I looked at a wide range of values for mtry, but I'm only looking
# at the best couple of values to make the code run faster for other 
# users
rfGrid08 &lt;- expand.grid(.mtry = 5:7)
rfFit08 &lt;- train(trainX08, 
                 trainY08, 
                 "rf", 
                 tuneGrid = rfGrid08, 
                 trControl = fitControl08, 
                 verbose = FALSE)
rfFit08
yhat08 &lt;- predict(rfFit08, newdata = testX08)
rfMse08 &lt;- mean(yhat08 != test08$y)
rfMse08

rfGrid09 &lt;- expand.grid(.mtry = 6:8)
rfFit09 &lt;- train(trainX09, 
                 trainY09, 
                 "rf", 
                 tuneGrid = rfGrid09, 
                 trControl = fitControl09, 
                 verbose = FALSE)
rfFit09
yhat09 &lt;- predict(rfFit09, newdata = testX09)
rfMse09 &lt;- mean(yhat09 != test09$y)
rfMse09


rfGrid10 &lt;- expand.grid(.mtry = 3:5)
rfFit10 &lt;- train(trainX10, 
                 trainY10, 
                 "rf", 
                 tuneGrid = rfGrid10, 
                 trControl = fitControl10, 
                 verbose = FALSE)
rfFit10
yhat10 &lt;- predict(rfFit10, newdata = testX10)
rfMse10 &lt;- mean(yhat10 != test10$y)
rfMse10

########################################################################
# Linear Discriminant Analysis (LDA)
########################################################################
# There's no parameter that we need to tune here but we do need to 
# check how well LDA performs 
ldaFit08 &lt;- train(trainX08, 
                  trainY08, 
                  "lda", 
                  trControl = fitControl08, 
                  verbose = FALSE)
ldaFit08
yhat08 &lt;- predict(ldaFit08, newdata = testX08)
ldaErr08 &lt;- mean(yhat08 != test08$y)
ldaErr08


ldaFit09 &lt;- train(trainX09, 
                  trainY09, 
                  "lda", 
                  trControl = fitControl09, 
                  verbose = FALSE)
ldaFit09
yhat09 &lt;- predict(ldaFit09, newdata = testX09)
ldaErr09 &lt;- mean(yhat09 != test09$y)
ldaErr09


ldaFit10 &lt;- train(trainX10, 
                  trainY10, 
                  "lda", 
                  trControl = fitControl10, 
                  verbose = FALSE)
ldaFit10
yhat10 &lt;- predict(ldaFit10, newdata = testX10)
ldaErr10 &lt;- mean(yhat10 != test10$y)
ldaErr10

########################################################################
# Support Vector Machines (SVM)
########################################################################
# I've already chosen the optimal tuning parameters just so that the 
# code runs faster.
svmGrid08 &lt;- expand.grid(.C = 2, .sigma = .001)
svmFit08 &lt;- train(trainX08, 
                  trainY08, 
                  "svmRadial", 
                  tuneGrid = svmGrid08, 
  		            trControl = fitControl08)
svmFit08

yhat08 &lt;- predict(svmFit08, newdata = testX08)
svmErr08 &lt;- mean(yhat08 != test08$y)
svmErr08


svmGrid09 &lt;- expand.grid(.C = 1, .sigma = 10^(-3))
svmFit09 &lt;- train(trainX09, 
                  trainY09, 
                  "svmRadial", 
                  tuneGrid = svmGrid09, 
    	            trControl = fitControl09)
svmFit09

yhat09 &lt;- predict(svmFit09, newdata = testX09)
svmErr09 &lt;- mean(yhat09 != test09$y)
svmErr09


svmGrid10 &lt;- expand.grid(.C = .5, .sigma = 10^(-2))
svmFit10 &lt;- train(trainX10, 
                  trainY10, 
                  "svmRadial", 
                  tuneGrid = svmGrid10, 
                  trControl = fitControl10)
svmFit10

yhat10 &lt;- predict(svmFit10, newdata = testX10)
svmErr10 &lt;- mean(yhat10 != test10$y)
svmErr10
