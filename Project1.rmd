---
title: "Practical_Machine_Learning_Project"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset).

## Data
The training data for this project are available here: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv]

The test data are available here: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv]

The data for this project come from this source: [http://groupware.les.inf.puc-rio.br/har]. If you use the document you create for this class for any purpose please cite them as they have been very generous in allowing their data to be used for this kind of assignment.

## Final Goal
The goal of your project is to predict the manner in which they did the exercise. This is the “classe” variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases.

## Reproducibility
Set your working directory
```{r}
setwd("C:/Users/Alex/Desktop/Coursera/Practical Machine Learning")
```

install the following:
```
install.packages("caret")
install.packages("dplyr")
install.packages("randomForest")
install.packages("e1071")
```

Load the following packages
```{r}
library(caret)
library(dplyr)
library(randomForest)
library(e1071)
```

## Creating Data set
Download the data from above in the "Data" section and copy into working directory

```{r}
rawtrain <- read.csv("pml-training.csv",na.strings=c("NA","","#DIV/0!"))
```

## splitting Data
Taking 70% for the training data and 30% for the test data

```{r}
set.seed(555)
inTrain <- createDataPartition(y = rawtrain$classe, list = FALSE, p=0.7)
trainData <- rawtrain[inTrain,]
testData <- rawtrain[-inTrain,]
```

## Identify N/A values
```{r}
table(is.na(trainData))
```
```{r}
naprops <- colSums(is.na(trainData))/nrow(trainData)
mostNAs <- names(naprops[naprops > 0.75])
mostNACols <- which(naprops > 0.75)
```

## Random sample from training data
Select a sample and remove N/A's
```{r}
set.seed(1256)
sampletrain <- trainData %>% tbl_df %>% sample_n(size=1000)
sampletrain <- sampletrain[,-mostNACols]
```

## Remove row number and user name
```{r}
sampletrain <- sampletrain[,-grep("X|user_name",names(sampletrain))]
```

## Remove the cvtd_timestamp variable
```{r}
sampletrain <- sampletrain[,-grep("cvtd_timestamp",names(sampletrain))]
```

##Remove candidate
```{r}
sampletrain <- sampletrain[,-nearZeroVar(sampletrain)]
```

##List of candidate predictors
```{r}
modelVars <- names(sampletrain)
modelVars1 <- modelVars[-grep("classe",modelVars)] 
modelVars
```

##Random Forest
```{r}
set.seed(57)
cleanedTrainData <- trainData[,modelVars]
modelFit <- randomForest(classe ~., data=cleanedTrainData, type="class")
```

## Error Estimates
```{r}
predTrain <- predict(modelFit,newdata=trainData)
confusionMatrix(predTrain,trainData$classe)$table
```
The in-sample error is high.

Now getting an out of sample error estimate
```{r}
classe_col <- grep("classe",names(testData))
predTest <- predict(modelFit, newdata = testData[,-classe_col], type="class")

confusionMatrix(predTest,testData$classe)
```

The model has an out of sample accuracy of: 0.998

##Prediciting exercise activity using the model
Loading the pml-test Data
```{r}
pmltest <- read.csv("pml-testing.csv",na.strings=c("NA","","#DIV/0!"))
```

##Perform Prediction
```{r}
predplmtest <- predict(modelFit, newdata = pmltest, type="class")
```

```{r}
predplmtest
```
