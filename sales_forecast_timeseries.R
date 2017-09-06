install.packages("forecast")
library(forecast)
install.packages("tseries")
library(tseries)
install.packages("TSA")
library(TSA)
sales<-read.csv(file.choose(),header = T)
sales
plot(sales$Sales~sales$Quarter,type='l')
train<-head(sales$Sales,12)
train
test<-(sales$Sales[13:16])
test
x<-diff(train)
adf.test(x,alternative ="stationary")
acf(x)
pacf(x)
m1<-arima(x,order = c(2,0,2),method = "ML")
m1
fitted(m1)
y<-diff(test)
y
d1=data.frame(y,f=forecast(m1,h=3))
d1
MAPE=function(a,f)
{
  u=mean(abs(1-(f/a)))*100
  return(u)
}
MAPE(a,f)
pred<-predict(m1,n.ahead = 1*4)
pred
