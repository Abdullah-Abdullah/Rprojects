#importing the data
Data = read.csv('E:/Imarticus/R/Data Sets/Supervised/Classification/Breast Cancer Data/Breast Cancer Data.csv',na.strings = c(""," ","NA"))

#EDA
summary(Data)
View(Data)

#Removing the unwanted variable
Data = Data[ ,-1]

Data$Bare.nuclei[is.na(Data$Bare.nuclei)] = mean(Data$Bare.nuclei,na.rm = T)
Data$Class = as.factor(Data$Class)

# splitting of the dataset into train and validate set 

library(caTools)
set.seed(100)
split<- sample.split(Data$Class , SplitRatio = 0.70)
train_set<-subset(Data,split ==TRUE)
val_set<-subset(Data,split ==FALSE)
View(val_set)

# building the model on train set and validating it on validation set 
attach(train_set)

# 1) Logistic regression model
BC = glm(Class~.,family = binomial(link = 'logit'), data = train_set)
summary(BC) #aic = 100

BC1 = glm(Class~Cl.thickness+Marg.adhesion+Bare.nuclei+Bl.cromatin,family = binomial(link = 'logit'),data = train_set)
summary(BC1) #aic = 102

#prediction
BC_Test1 = predict(BC1,val_set,type = 'response')
BC_Test1 = ifelse(BC_Test1>0.5,1,0)

#Confusion matrix
bc_cm1<-table(BC_Test1, val_set$Class)
bc_cm1

#accuracy
source('E:/Imarticus/R/accuracy_function.R')
acc(bc_cm1)

#############################################################################

# 2) SVM
library(e1071)
BC2<-svm(formula = Class~.,data = train_set, type = 'C-classification' ,kernel = 'radial')
BC3<-svm(formula = Class~.,data = train_set, type = 'C-classification' ,kernel = 'sigmoid')
BC4<-svm(formula = Class~.,data = train_set, type = 'C-classification' ,kernel = 'polynomial')

#prediction
BC_Test2 = predict(BC2,newdata = val_set,type='class')
BC_Test3 = predict(BC3,newdata = val_set,type='class')
BC_Test4 = predict(BC4,newdata = val_set,type='class')

#confusion matrix
bc_cm2<-table(BC_Test2, val_set$Class)
bc_cm3<-table(BC_Test3, val_set$Class)
bc_cm4<-table(BC_Test4, val_set$Class)

#accuracy
acc(bc_cm2) #91.02
acc(bc_cm3) #88.60
acc(bc_cm4) #97.01

############################################################################

# 3) KNN

library(class)
BC_Test5 = knn(train = train_set,test = val_set,cl=train_set$Class,k=100,prob = TRUE)

#confusion matrix
bc_cm5 = table(BC_Test5,val_set$Class)

#accuracy
acc(bc_cm5)

############################################################################

# 4) Naive Bayes classifier 

set.seed(5466)
BC6<-naiveBayes(Class~.,data = train_set)

# prediction 
BC_Test6<-predict(BC6,newdata = val_set)

#confusion matrix
bc_cm6<-table(BC_Test6,val_set$Class)
acc(bc_cm6)

#################################################################################

# 5) Decision Tree

library(rpart)
BC7 = rpart(formula = Class~., data = train_set)

# prediction 
BC_Test7 = predict(BC7 , newdata = val_set , type = 'class')

# confusion matrix
bc_cm7 = table(BC_Test7 , val_set$Class)
bc_cm7

# accuracy 
acc(bc_cm7)

#############################################################################

# 6) Random Forest

set.seed(1235)
library(randomForest)
BC8 = randomForest(x = train_set[-10], y = train_set$Class, ntree = 200)

# prediction 
BC_Test8<-predict(BC8,newdata = val_set)

# confusion matrix
bc_cm8<- table(BC_Test8,val_set$Class)
acc(bc_cm8)
