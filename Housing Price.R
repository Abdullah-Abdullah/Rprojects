#Housing price prediction

#Importing the files

hprice = read.csv('E:/Imarticus/R/Data Sets/housing price pred.csv')
View(hprice)
dim(hprice)

#removing insignificant columns
hprice = hprice[,-c(1,2)]

#Checking for missing values
colSums(is.na(hprice))

str(hprice)
hprice$waterfront = as.factor(hprice$waterfront)
hprice$view = as.factor(hprice$view)
hprice$condition = as.factor(hprice$condition)
hprice$grade = as.factor(hprice$grade)

head(hprice)
summary(hprice)
summary(log(hprice$price))

hprice = data.frame(hprice,pricel=log(hprice$price))

cor.test(hprice$bedrooms,hprice$pricel)
