#importing the data
Credit = read.csv('E:/Imarticus/R/Data Sets/Supervised/Classification/Credit Data/R_Module_Day_10.2_Credit_Risk_Train_data.csv',na.strings = c(""," ","NA"))
Test = read.csv('E:/Imarticus/R/Data Sets/Supervised/Classification/Credit Data/R_Module_Day_10.4_Credit_Risk_Validate_data.csv',na.strings = c(""," ","NA"))
Pred = read.csv('E:/Imarticus/R/Data Sets/Supervised/Classification/Credit Data/R_Module_Day_10.3_Credit_Risk_Test_external_data.csv',na.strings = c(""," ","NA"))

#EDA of Train Data
summary(Credit)
View(Credit)
str(Credit)
Credit = Credit[,-1]

#calling the mode function
source('E:/Imarticus/R/Mode Functions/Mode Function.R')
Credit$Gender[is.na(Credit$Gender)] = Mode1(Credit$Gender)
Credit$Married[is.na(Credit$Married)] = Mode1(Credit$Married)
Credit$Dependents[is.na(Credit$Dependents)] = Mode1(Credit$Dependents)
Credit$Self_Employed[is.na(Credit$Self_Employed)] = Mode1(Credit$Self_Employed)
Credit$LoanAmount[is.na(Credit$LoanAmount)] = median(Credit$LoanAmount,na.rm = T)
Credit$Loan_Amount_Term[is.na(Credit$Loan_Amount_Term)] = median(Credit$Loan_Amount_Term,na.rm = T)

#converting the numeric variable in factor
Credit$Credit_History = as.factor(Credit$Credit_History)
Credit$Credit_History[is.na(Credit$Credit_History)] = Mode1(Credit$Credit_History)

#Assumptions
chisq.test(Credit$Loan_Status,Credit$Gender) #reject
chisq.test(Credit$Loan_Status,Credit$Married)
chisq.test(Credit$Loan_Status,Credit$Dependents) #reject
chisq.test(Credit$Loan_Status,Credit$Education)
chisq.test(Credit$Loan_Status,Credit$Self_Employed) #reject
chisq.test(Credit$Loan_Status,Credit$Credit_History) 
chisq.test(Credit$Loan_Status,Credit$Property_Area)
t.test(Credit$ApplicantIncome~Credit$Loan_Status) #reject
t.test(Credit$CoapplicantIncome~Credit$Loan_Status) #reject
t.test(Credit$LoanAmount~Credit$Loan_Status) #reject
t.test(Credit$Loan_Amount_Term~Credit$Loan_Status) #reject

#EDA of Test Data
summary(Test)
Test = Test[,-1]

Test$Gender[is.na(Test$Gender)] = Mode1(Test$Gender)
Test$Married[is.na(Test$Married)] = Mode1(Test$Married)
Test$Dependents[is.na(Test$Dependents)] = Mode1(Test$Dependents)
Test$Self_Employed[is.na(Test$Self_Employed)] = Mode1(Test$Self_Employed)
Test$LoanAmount[is.na(Test$LoanAmount)] = median(Test$LoanAmount,na.rm = T)
Test$Loan_Amount_Term[is.na(Test$Loan_Amount_Term)] = median(Test$Loan_Amount_Term,na.rm = T)

#converting the numeric variable in factor
Test$Credit_History = as.factor(Test$Credit_History)
Test$Credit_History[is.na(Test$Credit_History)] = Mode1(Test$Credit_History)

#EDA of Pred Data
summary(Pred)
Pred = Pred[,-1]

Pred$Gender[is.na(Pred$Gender)] = Mode1(Pred$Gender)
Pred$Married[is.na(Pred$Married)] = Mode1(Pred$Married)
Pred$Dependents[is.na(Pred$Dependents)] = Mode1(Pred$Dependents)
Pred$Self_Employed[is.na(Pred$Self_Employed)] = Mode1(Pred$Self_Employed)
Pred$LoanAmount[is.na(Pred$LoanAmount)] = median(Pred$LoanAmount,na.rm = T)
Pred$Loan_Amount_Term[is.na(Pred$Loan_Amount_Term)] = median(Pred$Loan_Amount_Term,na.rm = T)

#converting the numeric variable in factor
Pred$Credit_History = as.factor(Pred$Credit_History)
Pred$Credit_History[is.na(Pred$Credit_History)] = Mode1(Pred$Credit_History)


# 1) Logistic Regression Model

#Building the model
Model1 = glm(Loan_Status~.,family = binomial(link = 'logit'),data = Credit)
summary(Model1)

Model2 = glm(Loan_Status~Married+Credit_History+Property_Area,family = binomial(link = 'logit'),data = Credit)
summary(Model2)

#Validation
Test1 = predict(Model2,Test,type = 'response')
Test1 = ifelse(Test1>0.5,1,0)

#Finding the accuracy using Confusion Matrix
cm1 = table(Test1,Test$outcome)

#calling accuracy function
source('E:/Imarticus/R/accuracy_function.R')
acc(cm1)

#How to do using confusionMatrix of 'caret' package
#Test1 = predict(Model2,Test,type = 'response')
#Test1 = as.factor(ifelse(Test1>0.5,1,0))

#levels(Test$outcome)=0:1 (outcome is in 'Y' or 'N')

#library(caret)
#confusionMatrix(Test1,Test$outcome)

#Plotting ROC curve
#library(ROCR)

#predict1 = prediction(Test2,Test1$outcome)
#predict2 = performance(predict1,measure = "tpr",x.measure = "fpr")
#plot(predict2)

#auc = performance(predict1,measure = "auc")
#auc@y.values[1]

#Validation of external data
pred_final = predict(Model2,Pred,type = 'response')
pred_final = ifelse(pred_final>0.5,1,0)

Pred = data.frame(Pred,Answer = pred_final)
View(Pred)

###################################################################################

# 2) SVM

#Building model using SVM on Credit History
#Calling the required library to run SVM
library(e1071)

#Checking for outliers 
boxplot(Credit$ApplicantIncomer,Credit$CoapplicantIncome,Credit$LoanAmount,Credit$Loan_Amount_Term)
#Ignoring the outliers considering the nature of the variable

#Building the model

#Kernel-radial
Model3<-svm(formula=Loan_Status~ +Married+Property_Area+Credit_History,data=Credit)
summary(Model3)

#Kernel-linear
Model4<-svm(formula=Loan_Status~+Married+Property_Area+Credit_History,data=Credit,kernel='linear')
summary(Model4)

#Kernel-sigmoidal
Model5<-svm(formula=Loan_Status~+Married+Property_Area+Credit_History,data=Credit,kernel='sigmoid')
summary(Model5)

#Kernel-polynomial
Model6<-svm(formula=Loan_Status~+Married+Property_Area+Credit_History,data=Credit,kernel='polynomial')
summary(Model6)

#Validation 
#For Model3
Test2 = predict(Model3,newdata = Test,type='class')
cm2 = table(Test2,Test$outcome)
acc(cm2)

#For Model4
Test3 = predict(Model4,newdata = Test,type='class')
cm3 = table(Test3,Test$outcome)
acc(cm3)

#For Model5
Test4 = predict(Model5,newdata = Test,type='class')
cm4 = table(Test4,Test$outcome)
acc(cm4)

#For Model6
Test5 = predict(Model6,newdata = Test,type='class')
cm5 = table(Test5,Test$outcome)
acc(cm5)

#################################################################################

# 3) KNN - K Nearest Neighbour

library(class)

#Building the model

n = 500
Model7 = knn(train = Credit[-12],test = Test,cl = Credit$Loan_Status,k=100,prob = T)
dim(Credit)
dim(Test)
colSums(is.na(Test))
#################################################################################

# 4) Naive Bayes classifier 

set.seed(5465)
Model8<-naiveBayes(Loan_Status~.,data = Credit)

# prediction 
Test7<-predict(Model8,newdata = Test)

#confusion matrix
cm7<-table(Test7,Test$outcome)
acc(cm7)

#################################################################################

# 5) Decision Tree

library(rpart)
Model9 = rpart(formula = Loan_Status~., data = Credit)

# prediction 
Test8 = predict(Model9 , newdata = Test , type = 'class')


# confusion matrix
cm8 = table(Test8 , Test$outcome)
cm8

# accuracy 
acc(cm8)

#############################################################################

# 6) Random Forest

set.seed(1234)
library(randomForest)
Model10 = randomForest(x = Credit[-12], y = Credit$Loan_Status, ntree = 200)

# prediction 
Test9<-predict(Model10,newdata = Test)

# confusion matrix
cm9<- table(Test9,Test$outcome)
acc(cm9)
