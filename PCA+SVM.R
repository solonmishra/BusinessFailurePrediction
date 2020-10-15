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


library(caret)
nzv_totaldata<- total_data[, c(1:64)]
nzv<- nearZeroVar(nzv_totaldata, saveMetrics = TRUE)
print(paste('Range:', range(nzv$percentUnique)))
head(nzv)




######PCA+SVM####
set.seed(123)
split = createDataPartition(total_data$class, p=0.7, list=FALSE)
train = total_data[split,]
test  = total_data[-split,]

train[,-65] = scale(train[,-65])
test[,-65]  = scale(test[,-65])


pca = preProcess(train[,-65], 
                 method='pca',
                 pcaComp = 10)

train = predict(pca, train)   # column orders change. The DV becomes the first variable
test =  predict(pca, test)

train = train[,c(2,3,4,5,6,7,8,9,10,11,1)]
test  = test[,c(2,3,4,5,6,7,8,9,10,11,1)]

modSVM = svm(class ~ .,data=train,
             type='C-classification',
             kernel='radial')
y_pred = predict(modSVM, test[,-11])
confusionMatrix(y_pred, test[,11])


set = train

X1 = seq(from=min(set[,1])-1, to=max(set[,1]+1), by=0.02)
X2 = seq(from=min(set[,2])-1, to=max(set[,2]+1), by=0.02)
grid_set = expand.grid(X1, X2)

colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(modSVM, grid_set)

plot(set[,-3],
     main = 'SVM after Principal Component Analysis',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))

#contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add=TRUE)   # this is optional

#points(grid_set, pch='.',col=ifelse(y_grid==2,'deepskyblue',ifelse(y_grid==1,'springgreen3','tomato')))
#points(set, pch=21, bg=ifelse(set[,3]==2,'blue3', ifelse(set[,3]==1, 'green4','red3')))


# Visualize the decision boundaries for test set
set = test

X1 = seq(from=min(set[,1])-1, to=max(set[,1]+1), by=0.02)
X2 = seq(from=min(set[,2])-1, to=max(set[,2]+1), by=0.02)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('PC1', 'PC2')
y_grid = predict(modSVM, grid_set)

plot(set[,-3],
     main = 'SVM after Principal Component Analysis',
     xlab = 'PC1', ylab = 'PC2',
     xlim = range(X1), ylim = range(X2))

#contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add=TRUE)   # this is optional

#points(grid_set, pch='.',col=ifelse(y_grid==2,'deepskyblue',ifelse(y_grid==1,'springgreen3','tomato')))
#points(set, pch=21, bg=ifelse(set[,3]==2,'blue3', ifelse(set[,3]==1, 'green4','red3')))


################Applying K fold
library(caret)
folds = createFolds(train$class, k = 5)
cv = lapply(folds, function(x) {
        training_fold = train[-x, ]
        test_fold = train[x, ] 
        classifier = svm(formula = class ~ .,
                         data = training_fold,
                         type = 'C-classification',
                         kernel = 'radial')
        y_pred = predict(classifier, newdata = test_fold[-11])
        #confusionMatrix(test_fold[, 65], y_pred)
        cm = table(test_fold[, 11], y_pred)
        accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
        return(accuracy)
})

knitr::include_graphics("CV.png")
cv
accuracy = mean(as.numeric(cv))
accuracy







