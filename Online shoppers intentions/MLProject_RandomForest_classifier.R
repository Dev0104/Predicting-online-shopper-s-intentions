library(mice)
library(VIM)

#Reading the file
online_shoppers <- read.csv("D:\\DMML\\Data Sets\\online_shoppers_intention.csv",header = TRUE,sep = ',')
summary(online_shoppers)
str(online_shoppers)
online_shoppers$Revenue = as.factor(online_shoppers$Revenue)
table(online_shoppers$Revenue)

#Verifying missing values
md.pattern(online_shoppers)

#data Partitiion
set.seed(123)
ind = sample(2,nrow(online_shoppers),replace = TRUE, prob = c(0.7,0.3))
train = online_shoppers[ind == 1,]
test = online_shoppers[ind == 2,]


#Applying random forest
library(randomForest)
set.seed(222)
rf = randomForest(Revenue~ExitRates+PageValues+Month,data = train, importance = TRUE, proximity = TRUE,do.trace=T)
print(rf)
attributes(rf)

#Prediction & Confusion matrix - train data
library(caret)
p1=predict(rf,train)
confusionMatrix(p1,train$Revenue)

#Prediction & Confusion matrix - test data
p2=predict(rf,test)
confusionMatrix(p2,test$Revenue,mode = 'everything')


#Error rate of Random Forest
par(1,1)
plot(rf)


#tune MTRY
#t = tuneRF(train[,-18], train[,18], stepFactor = 0.5, plot = TRUE, ntreeTry = 300, trace = TRUE, improve = 0.05)

#Histogram
hist(treesize(rf))


#Variable importance
varImpPlot(rf,sort = T)
importance(rf)
varUsed(rf)


#Partial Dependence plot
partialPlot(rf,train,PageValues,FALSE)

#Extract Single tree
#getTree(rf,1,labelVar = TRUE)

#Multi Dimensional plot of Proximity matrix
#MDSplot(rf,train$Revenue)

