# Final_Assignment_ML
Final assignment in ML course in courera

setwd("G:\\04 year\\rubbish")

data1=read.csv("pml-training.csv",sep = ",",header = T) #train
data2=read.csv("pml-testing.csv",sep = ",",header = T) #test

#data partition
set.seed(222)
ind=sample(2,nrow(data1),replace = T,prob = c(0.8,0.2))
train=data1[ind==1,]
test=data1[ind==2,]

# Decision tree 

library(rpart)
library(rpart.plot)

set.seed(222)
model1=rpart(classe ~ ., data = train, method = "class")

# confusion matrix
predi1=predict(model1, test, type = "class")
conf1=confusionMatrix(predi1,test$classe)
conf1

# Random Forest
library(caret)

set.seed(222)
control=trainControl(method = "cv", number = 3, verboseIter=FALSE)
model2=train(classe ~ ., data = train, method = "rf", trControl = control)


# confusion matrix
predi2=predict(model2, test, type = "class")
conf2=confusionMatrix(predi2,test$classe)
conf2



#prediction for testing set

predi_final=predict(model1, data2)
predi_final
