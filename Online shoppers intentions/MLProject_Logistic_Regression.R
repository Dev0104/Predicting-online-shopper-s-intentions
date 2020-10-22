install.packages("cranly")
library(cranly)
online_shoppers <- read.csv("D:\\DMML\\Data Sets\\online_shoppers_intention.csv",header = TRUE,sep = ',')
summary(online_shoppers)
str(online_shoppers)


#Data Processing
online_shoppers$Revenue = as.factor(online_shoppers$Revenue)
online_shoppers$OperatingSystems = as.factor((online_shoppers$OperatingSystems))
online_shoppers$Browser = as.factor(online_shoppers$Browser)
online_shoppers$Region = as.factor(online_shoppers$Region)
online_shoppers$TrafficType = as.factor(online_shoppers$TrafficType)
#xtabs(~Revenue+OperatingSystems+Browser+Region+TrafficType, data = online_shoppers)


table(online_shoppers$Revenue)

#Verifying missing values
library(mice)
md.pattern(online_shoppers)

#Data partition
set.seed(1234)
ind = sample(2,nrow(online_shoppers),replace = T,prob=c(0.8,0.2))
train = online_shoppers[ind==1,]
test = online_shoppers[ind==2,]

#Logistic model
mymodel = glm(Revenue~.,data=train, family = 'binomial')
summary(mymodel)
mymodel2 = glm(Revenue~ExitRates+PageValues+Month,data=train, family = 'binomial')
summary(mymodel2)


#Prediction - train
p1 = predict(mymodel2,train,type = 'response')
head(p1)
head(train)
#View(test)

#Misclassification error - train data
pred1 = ifelse(p1>0.5,TRUE,FALSE)
tab1 = table(predicted = pred1,actual = train$Revenue)
tab1
1-sum(diag(tab1))/sum(tab1) 


#Prediction - test
p2 = predict(mymodel2,test,type = 'response')
head(p2)
head(test)


#Misclassification error - test data
pred2 = ifelse(p2>0.5,TRUE,FALSE)
tab2 = table(predicted = pred2,actual = test$Revenue)
tab2
1-sum(diag(tab2))/sum(tab2)
library(caret)
confusionMatrix(tab2,mode='everything')


#Goodness of fit
with(mymodel2,pchisq(null.deviance - deviance,df.null-df.residual,lower.tail = F))
     
