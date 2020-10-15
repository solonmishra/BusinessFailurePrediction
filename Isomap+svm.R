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


######## IsOMAP #####
library(dimRed)
iso_data = Isomap(data= total_data, dims=2, k=10)
head(simData_dim2_IM$dim2)