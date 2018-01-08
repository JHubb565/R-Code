#Question 3.4

library("astsa", lib.loc="~/R/win-library/3.2")
library("TSA", lib.loc="~/R/win-library/3.2")

abs(-6) #Confirm Absolute Value

x=arima.sim(list(order=c(1,0,0), ar=.7), n=100) #Time series AR 1 with n=100

plot.ts(arima.sim(list(order=c(1,0,0), ar=.7), n=100), ylab='x') #Plot our time series

acf2(x) #ACF and PACF 

