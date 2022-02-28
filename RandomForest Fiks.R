
#Load Data Set#
data=read.csv(file.choose(),sep = ";")

#Eksplorasi Data#
str(data)
dim(data)
head(data)

#Visualisasi Data#
counts=table(data$group)
counts
par(mfrow=c(1,1))
barplot(counts,main = "Barplot of Group",xlab = "Group",ylab = "Counts",col = c("violetred","peachpuff4","lightpink","rosybrown"))


#Mengubah nilai NA menjadi 0#
library(car)
d=as.data.frame(data)
d
d[is.na(d)]=0
head(d)

#pengubahan tipe data
group=as.data.frame(d$group)
group
group1=lapply(group,as.factor)
str(group1)

data1=d[,3:219]
str(data1)

#penggabungan data frame
data2=data.frame(group1,data1)
str(data2)

#Melakukan klasifikasi Random Forest#
install.packages("dplyr")
install.packages("randomForest")
install.packages("caret")
library(dplyr)
library(randomForest)
library(caret)

set.seed(100)
train=sample(nrow(data2),0.6*nrow(data2),replace = FALSE)
trainset=data2[train,]
validset=data2[-train,]
summary(trainset)
summary(validset)

#Membentuk model#
model1=randomForest(d.group~.,data = trainset, importance=TRUE)
model1

# Fine tuning parameters of Random Forest model
model2 =randomForest(d.group~.,data = trainset,ntree = 500, mtry=14, importance =TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, trainset, type = "class")

# Checking classification accuracy
table(predTrain, trainset$d.group)  

# Predicting on Validation set
predValid <- predict(model2, validset, type = "class")

# Checking classification accuracy
mean(predValid == validset$d.group)                    
table(predValid,validset$d.group)

# To check important variables
importance(model2)   

#menampilkan output model random forest
varImpPlot(model2)        
plot(model2)

#menampilkan hasil tree
install.packages("devtools")
library(devtools)
devtools::install_github('skinner927/reprtree')
library(reprtree)


reprtree:::plot.getTree(model2)

