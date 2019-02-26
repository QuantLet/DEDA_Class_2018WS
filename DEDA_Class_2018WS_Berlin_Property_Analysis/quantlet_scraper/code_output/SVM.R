library(e1071)
library(ggplot2)
#read excel file
mydata &lt;-read.csv("factorsall.csv",header=TRUE)
md&lt;-mydata[,c(6,5,40,41,42)]
#plot PC1 and PC2
qplot(Factor1,Factor2,data=md,color=type)
#SVM model1(radial)
mymodel&lt;-svm(type~.,data=md)
summary(mymodel)
plot(mymodel,data=md,
     Factor2~Factor1,
     )
pred&lt;-predict(mymodel,md)
tab&lt;-table(Predicted=pred,Actual=md$type)
tab
1-sum(diag(tab))/sum(tab)
#SVM model2(linear)
mymodel1&lt;-svm(type~.,data=md,kernel="linear")
summary(mymodel1)
plot(mymodel1,data=md,
     Factor2~Factor1,
)
pred&lt;-predict(mymodel1,md)
tab&lt;-table(Predicted=pred,Actual=md$type)
tab
1-sum(diag(tab))/sum(tab)
#SVM model3(polynomial)
mymodel2&lt;-svm(type~.,data=md,kernel="polynomial")
summary(mymodel2)
plot(mymodel2,data=md,
     Factor2~Factor1,
)
pred&lt;-predict(mymodel2,md)
tab&lt;-table(Predicted=pred,Actual=md$type)
tab
1-sum(diag(tab))/sum(tab)
#SVM model4(sigmoid)
mymodel3&lt;-svm(type~.,data=md,kernel="sigmoid")
summary(mymodel3)
plot(mymodel3,data=md,
     Factor2~Factor1,
)
pred&lt;-predict(mymodel3,md)
tab&lt;-table(Predicted=pred,Actual=md$type)
tab
1-sum(diag(tab))/sum(tab)
#choose best model
#tuning
set.seed(123)
tmodel&lt;-tune(svm,type~.,data=md,
             ranges=list(epsilon=seq(0,1,0.1),cost=2^(6:9)))
plot(tmodel)
summary(tmodel)
#best Model
mymodel&lt;-tmodel$best.model
summary(mymodel)
plot(mymodel,data=md,
     Factor2~Factor1,
)
pred&lt;-predict(mymodel,md)
tab&lt;-table(Predicted=pred,Actual=md$type)
tab
1-sum(diag(tab))/sum(tab)
