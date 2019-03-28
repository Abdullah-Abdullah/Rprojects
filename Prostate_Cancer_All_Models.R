
library(class)
library(caret)


#importing the data
pc_data = read.csv('E:/Imarticus/R/Data Sets/Supervised/Classification/Prostate Cancer/Prostate_Cancer.csv',na.strings = c(""," ","NA"))
summary(pc_data)
View(pc_data)

#removing irrelevent coloumns
pc_data = pc_data[,-1]
boxplot(pc_data)


#Normalising numeric data
normalize = function(x) {return ((x - min(x)) / (max(x) - min(x))) }

#normalised data for prostate cancer
pc_n = as.data.frame(lapply(pc_data[2:9],normalize))
boxplot(pc_n)
View(pc_n)

#creating training and test data
pc_train = pc_n[1:65,]
pc_test = pc_n[66:100,]
View(pc_train)

pc_train_labels = pc_data$diagnosis_result[1:65]
pc_test_labels = pc_data$diagnosis_result[66:100]

# 1) KNN

#training the model
#n = no of observations
n = 100
pc_test1 = knn(train=pc_train,test=pc_test,cl=as.factor(pc_train_labels),k=sqrt(n))

#table(pc_test_pred,pc_test_labels)
#pred_dat=cbind(pc_test,pc_test_pred,pc_test_labels)
#View(pred_dat)

#error = mean(pc_test_pred!=pc_test_labels)
#accuracy = 1-error
#accuracy

confusionMatrix(pc_test1,pc_test_labels)

###################################################################################

# 2) Naive Bayes
library(e1071)
pc2 = naiveBayes(pc_train_labels~.,data = pc_train)
pc2
pc_test2 = predict(pc2,newdata = pc_test)

#table(nb1,pc_test_labels)
confusionMatrix(pc_test2,pc_test_labels)

###################################################################################

# 3) Decision Tree

library(rpart)
pc3 = rpart(formula = pc_train_labels~., data = pc_train)

# prediction 
pc_test3 = predict(pc3 , newdata = pc_test , type = 'class')

confusionMatrix(pc_test3,pc_test_labels)

###################################################################################

# 4) Random Forest

set.seed(1238)
library(randomForest)
pc4 = randomForest(x = pc_train, y = pc_train_labels, ntree = 200)

# prediction 
pc_test4<-predict(pc4,newdata = pc_test)

confusionMatrix(pc_test4,pc_test_labels)

###################################################################################

# 5) SVM
library(e1071)

pc5<-svm(formula = pc_train_labels~.,data = pc_train, type = 'C-classification' ,kernel = 'radial')
pc6<-svm(formula = pc_train_labels~.,data = pc_train, type = 'C-classification' ,kernel = 'sigmoid')
pc7<-svm(formula = pc_train_labels~.,data = pc_train, type = 'C-classification' ,kernel = 'polynomial')

#prediction
pc_test5 = predict(pc5,newdata = pc_test,type='class')
pc_test6 = predict(pc6,newdata = pc_test,type='class')
pc_test7 = predict(pc7,newdata = pc_test,type='class')

confusionMatrix(pc_test5,pc_test_labels)
confusionMatrix(pc_test6,pc_test_labels)
confusionMatrix(pc_test7,pc_test_labels)

##################################################################################

# 6) Logistic Regression Model

pc8 = glm(pc_train_labels~.,family = binomial(link = 'logit'), data = pc_train)

#prediction
pc_test8 = predict(pc8,pc_test,type = 'response')
pc_test8 = as.factor(ifelse(pc_test8>0.5,1,0))

levels(pc_test_labels)=0:1
confusionMatrix(pc_test8,pc_test_labels)

