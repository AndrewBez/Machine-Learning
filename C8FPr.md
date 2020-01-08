---
title: "Prediction of Exercise Correctness"
author: Andrew Bez
output: 
  html_document:
    keep_md: true
---

# Summary



In this study we use the data from http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har to train the model to predict correctness of execution of physical exercise. We explore several models and find that random forest performs the best with 100% accuracy on the test dataset.

# Preparation and Data Cleaning

We start with loading the needed libraries and setting random number generator to ensure reproducibility

```r
library(caret); library(ggplot2); library(RCurl); set.seed(1234)
```
Next we download and read training and testing datasets

```r
urltrain <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(urltrain,destfile = "./pmltrain.csv", method="curl")
train <- read.csv("./pmltrain.csv")
urltest <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(urltest,destfile = "./pmltest.csv", method="curl")
test <- read.csv("./pmltest.csv")
```
A quick look at the data reveals that many variables have most of the data (over 95%) missing. Imputing the missing data with such a high percentage missing would not be the best approach as the variable values would mostly consist of single imputed value then. This would make predictive power of imputed variables negligible.
So instead we are going to drop the variables with mostly missing values. We choose 70% missing to be the threshold of what we are going to drop.

```r
trainshort <- train[,1:7]
for (i in 8:length(names(train))) {
    if (sum(is.na(train[,i]) | train[,i]=="")/length(train[,i])<.3) {
       trainshort <- cbind(trainshort,currentname=train[,i])
       names(trainshort)[length(names(trainshort))] <- names(train)[i]
    }
}
testshort <- test[,1:7]
for (i in 8:length(names(test))) {
    if (sum(is.na(test[,i]) | test[,i]=="")/length(test[,i])<.3) {
        testshort <- cbind(testshort,currentname=test[,i])
        names(testshort)[length(names(testshort))] <- names(test)[i]
    }
}
```
Next we split the training set into training and validation parts at 75%-25%. We also remove the variables that bear no predictive value due to either being irrelevant or being prone to confounding. These are the first 7 variables.

```r
indexTrain <- createDataPartition(trainshort$X,p=.75,list = F)
trn <- trainshort[indexTrain,-c(1:7)]
val <- trainshort[-indexTrain,-c(1:7)]
```
Similarly we remove these variables from the test set

```r
testshort1 <- testshort[,-c(1:7)]
```

# Modeling

Now we can fit several model and compare their performance on the validation set.

1. Random forest

```r
fit1 <- train(classe~.,data=trn,method="rf",prox=T)
pred1 <- predict(fit1,val)
confusionMatrix(pred1,val$classe)
```
produces accuracy of 0.9945

2. Gradient boosting

```r
fit2 <- train(classe~.,data=trn,method="gbm",verbose=F)
pred2 <- predict(fit2,val)
confusionMatrix(pred2,val$classe)
```
is a little behind with the accuracy of 0.9623.

3. Single regression tree

```r
fit3 <- train(classe~.,data=trn,method="rpart")
pred3 <- predict(fit3,val)
confusionMatrix(pred3,val$classe)
```
is far behind with the accuracy of 0.4994.

4. Linear discriminant analysis

```r
fit4 <- train(classe~.,data=trn,method="lda")
pred4 <- predict(fit4,val)
confusionMatrix(pred4,val$classe)
```
produces accuracy of 0.7009.

5. Naive Bayes

```r
fit5 <- train(classe~.,data=trn,method="nb")
pred5 <- predict(fit5,val)
confusionMatrix(pred5,val$classe)
```
produces accuracy of 0.7388.

Thus we see that random forest performs the best. 

# Testing

We apply the model that performed best, random forest

```r
predrf <- predict(fit1,testshort1)
res <- cbind(testshort1,predrf)
results <- res[,53:54]
```
The prediction turns out to be 100% accurate according to the quiz results.

Interestingly, gradient boosting model also produces 100% accuracy on test data

```r
predgbm <- predict(fit2,testshort1)
results_both <- cbind(results,predgbm)
```
