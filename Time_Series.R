library(readxl)
library(forecast)
mv = read_excel('E:/Imarticus/R/Data Sets/Stock price.xlsx')
View(mv)

n = nrow(mv)
mv = mv$Close[n:1]
plot(mv,type="l")

#diff() calculates difference of a series: to transform a series
plot(diff(mv),type = "l",main = "Original data")
acf(diff(mv))

myts = ts(mv,frequency = 12,start = c(2010,1))
myts

#plot series
plot(myts)

#Arima models
fit = auto.arima(myts)

#predict next 12 future values
forecast(fit,24)
