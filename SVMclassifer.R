
library(caret)
library(RDRToolbox)
library(vegan)
library(doSNOW)
library(caTools)
library(SparkR)
library(factoextra)
library(foreign)
library(e1071)
library(tidyr)
library(dplyr)
library(gdata)
library(caret)
library(ggplot2) 
library(ggfortify)
library(rlang)
library(dimRed)

set.seed(2)
data1<- read.arff("5year.arff")
data2<- read.arff("4year.arff")
data3<- read.arff("3year.arff")
data4<- read.arff("2year.arff")
data5<- read.arff("1year.arff")

data1 <- drop_na(data1)
data2 <- drop_na(data2)
data3 <- drop_na(data3)
data4 <- drop_na(data4)
data5 <- drop_na(data5)

data1 <- data1[2828:3031,]
data2 <- data2[4530:4769,]
data3 <- data3[4672:4885,]
data4 <- data4[3943:4088,]
data5 <- data5[3135:3195,]


total_data<- rbind(data1, data2, data3, data4, data5)
sum(is.na(total_data))
total_data<- drop_na(total_data)
any(is.na(total_data))


library(e1071)
library(caret)
set.seed(100)

split = createDataPartition(total_data$class, p=0.7, list=FALSE)
train = total_data[split,]
test  = total_data[-split,]

svm_model<- svm(class~ .,
                data = train,
                type = 'C-classification',
                kernel = 'radial',
                scale = TRUE)
summary(svm_model)

prediction = predict(svm_model, test) 
tab<- table(Predited= prediction, Actual= test$class )
tab
confusionMatrix(prediction, factor(test$class))


####Applying cross validation
library(caret)
folds = createFolds(train$class, k = 5)
cv = lapply(folds, function(x) {
  training_fold = train[-x, ]
  test_fold = train[x, ] 
  classifier = svm(formula = class ~ .,
                   data = training_fold,
                   type = 'C-classification',
                   kernel = 'radial')
  y_pred = predict(classifier, newdata = test_fold[-65])
  #confusionMatrix(test_fold[, 65], y_pred)
  cm = table(test_fold[, 65], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})

knitr::include_graphics("CV.png")
cv
accuracy = mean(as.numeric(cv))
accuracy
