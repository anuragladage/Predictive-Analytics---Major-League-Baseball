library(ElemStatLearn)
library(DMwR) #Missing Values
library(e1071)
library(kernlab)

mlbtraincomplete <- read.csv('/Users/anuragladage/Dropbox/Principles of Statistica Data Mining/Project/MLB_NEW/MLBALLtrain.csv', header = T)
mlbtestcomplete <- read.csv('/Users/anuragladage/Dropbox/Principles of Statistica Data Mining/Project/MLB_NEW/MLBALLtest.csv', header = T)

mlbtrain_NA <- mlbtraincomplete[,3:48]
mlbtrain <- centralImputation(mlbtrain_NA)
mlbtrain.x <- mlbtrain[,1:45]
mlbtrain.y <- mlbtrain[,46]
mlbtest <- mlbtestcomplete[,3:48]
mlbtest.x <- mlbtest[,1:45]
mlbtest.y <- mlbtest[,46]


#Rattle
library(rattle)
rattle()


#Logistic Regression
mlbtrain.logit.fit <- glm(Result~.,mlbtrain, family= binomial('logit'))
mlbtrain.step.prob <- predict(mlbtrain.logit.fit,mlbtest.x, type = 'response')
mlbtrain.step.yhat <- as.factor(ifelse(mlbtrain.step.prob > 0.5, '1', '0'))
mlbtrain.step.conf <- table(mlbtest.y,mlbtrain.step.yhat)
mlbtrain.step.accuracy <- sum(diag(mlbtrain.step.conf))/150
mlbtrain.step.accuracy

#Support Vector Machine
svm.gene <- ksvm(Result~., data=mlbtrain,kernel="polydot", C=60,cross=10, type = 'C-svc')
svm.est.class.gene <- predict(svm.gene, mlbtest.x, type = 'response')
mlbtest.svm.yhat <- as.factor(ifelse(svm.est.class.gene > 0.5, '1', '-1'))
table(mlbtest.y, mlbtest.svm.yhat)


#Performing QDA on the data
mlbtest.n <- 150  #Number of repeatitions
mlbtrain.qda.fit <- qda(Result~., data = mlbtrain)
mlbtest.qda.yhat <- predict(mlbtrain.qda.fit, mlbtest.x)$class
mlbtest.qda.conf <- table(mlbtest.y,mlbtest.qda.yhat)
mlbtest.qda.accuracy <- sum(diag(hw1data2.qda.conf))/mlbtest.n







View(mlbtraincomplete)
View(mlbtestcomplete)
View(mlbtrain)
View(mlbtest)