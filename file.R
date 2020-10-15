#The observations in the data are Polish companies with 64 explanatory, which are financial ratios of a company and 1 binary response variable, 
#which is 1 if the company filled for banruptcy or 0 if not,

#We also delete some observations so that the numbers of bankrupts is equal to non-bankrupts. 
#Apparently, the accuracy of the model is better if the test dataset is made of smaller number of bankrupts.

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

#write.csv(total_data, file = "total.csv", row.names = F)

library(ggcorrplot)
ggcorrplot(cor(total_data[,-65]))

qplot(total_data$Attr1, total_data$Attr2, data = total_data,
      color = class)

str(total_data)
head(total_data)
colnames(total_data)<- c("net profit / total assets",
                      "total liabilities / total assets",
                      "working capital / total assets",
                      "current assets / short-term liabilities",
                      "[(cash + short-term securities + receivables - short-term liabilities) / (operating expenses - depreciation)] * 365",
                      "retained earnings / total assets",
                      "EBIT / total assets ",
                      "book value of equity / total liabilities ",
                      "sales / total assets ",
                      "equity / total assets ",
                      "(gross profit + extraordinary items + financial expenses) / total assets",
                      "gross profit / short-term liabilities ",
                      "(gross profit + depreciation) / sales ",
                      "(gross profit + interest) / total assets ",
                      "(total liabilities * 365) / (gross profit + depreciation) ",
                      "(gross profit + depreciation) / total liabilities ",
                      "total assets / total liabilities ",
                      "gross profit / total assets ",
                      "gross profit / sales ",
                      "(inventory * 365) / sales ",
                      "sales (n) / sales (n-1) ",
                      "profit on operating activities / total assets ",
                      "net profit / sales ",
                      "gross profit (in 3 years) / total assets ",
                      "(equity - share capital) / total assets ",
                      "(net profit + depreciation) / total liabilities ",
                      "profit on operating activities / financial expenses ",
                      "working capital / fixed assets ",
                      "logarithm of total assets ",
                      "(total liabilities - cash) / sales ",
                      "(gross profit + interest) / sales ",
                      "(current liabilities * 365) / cost of products sold ",
                      "operating expenses / short-term liabilities ",
                      "operating expenses / total liabilities ",
                      "profit on sales / total assets ",
                      "total sales / total assets ",
                      "(current assets - inventories) / long-term liabilities ",
                      "constant capital / total assets ",
                      "profit on sales / sales ",
                      "(current assets - inventory - receivables) / short-term liabilities ",
                      "total liabilities / ((profit on operating activities + depreciation) * (12/365)) ",
                      "profit on operating activities / sales ",
                      "rotation receivables + inventory turnover in days ",
                      "(receivables * 365) / sales ",
                      "net profit / inventory ",
                      "(current assets - inventory) / short-term liabilities ",
                      "(inventory * 365) / cost of products sold ",
                      "EBITDA (profit on operating activities - depreciation) / total assets ",
                      "EBITDA (profit on operating activities - depreciation) / sales ",
                      "current assets / total liabilities ",
                      "short-term liabilities / total assets ",
                      "(short-term liabilities * 365) / cost of products sold) ",
                      "equity / fixed assets ",
                      "constant capital / fixed assets ",
                      "working capital ",
                      "(sales - cost of products sold) / sales ",
                      "(current assets - inventory - short-term liabilities) / (sales - gross profit - depreciation) ",
                      "total costs /total sales ",
                      "long-term liabilities / equity ",
                      "sales / inventory ",
                      "sales / receivables ",
                      "(short-term liabilities *365) / sales ",
                      "sales / short-term liabilities ",
                      "sales / fixed assets", "class"
)

#total_data$class<- ifelse(test = total_data$class==0, yes = "success", no = "failure")
#total_data$class<- as.factor(total_data$class)
str(total_data)
summary(total_data$class)



################PCA#############


bfp_pc<- cbind(pca.data[,-1], total_data$class)


######## PCA+SVM ##############
set.seed(123)
split = createDataPartition(total_data$class, p=0.7, list=FALSE)
train = total_data[split,]
test  = total_data[-split,]

#samples<- total_data[, c(1:64)]
pc<- princomp(total_data[,c(1:64)], cor= TRUE, score= TRUE)
summary(pc)
plot(pc)
library(graphics)
plot(pc, type= "l")
pc$loadings
pc$scores

pca<- prcomp(total_data[,c(1:64)] , center = TRUE, scale = TRUE )
print(pca)
summary(pca)
get_eig(pca)

#library(devtools)
#install_github("vqv/ggbiplot")


library(ggplot2)
qplot(pca$x[,1], pca$x[,2], data= pca, color= "red", "blue")
pca.var<- pca$sdev^2
pca.var.per<- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main = "Scree plot", xlab = "principal component", ylab = "percent variation")
summary(pca)

pca.data<- data.frame( sample= rownames(pca$x), 
                       X= pca$x[,1],
                       Y= pca$x[,2])

ggplot(data = pca.data, aes(x= X, y=Y, label = sample))+
  geom_text()  +
  xlab(paste("PC1- ", pca.var.per[1], "%", sep = ""))+
  ylab(paste("PC2- ", pca.var.per[2], "%", sep = ""))+
  theme_bw()+
  ggtitle("PCA graph")

loadingscores<- pca$rotation[,1]
att_scores<- abs(loadingscores)
att_score_ranked<- sort(att_scores, decreasing = TRUE)
top_att<- names(att_score_ranked[1:10])
top_att
pca$rotation[top_att,1] ##show the score (+/- sign)
#BFP_pc<- total_data[,c(14,7,18,38,2,10,1,25,6,22,65)]
bf_pc<- pca$x[,c(14,7,18,38,2,10,1,25,6,22)]

train = predict(pca, train)   # column orders change. The DV becomes the first variable
test =  predict(pca, test)


modSVM = svm(total_data$class ~ .,data= bf_pc,
             type='C-classification',
             kernel='radial')
y_pred = predict(modSVM, test[,-3])
confusionMatrix(y_pred, test[,3])

