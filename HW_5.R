x=ts(data=c(-2.72,-2.93,-3,-2.29,-1.95,-.04))
mean(x)
install.packages("TSA")
library(TSA)
plot.ts(x)
ar(x)
?ar
data=data.frame(x)
ar(data,order.max=1,method=c("yule-walker"))
arima(data,method=c("CSS"))
data("ar1.2.s")
arima(data,order=c(1,0,0),method=c("CSS"), include.mean = FALSE)



x=ts(data=c(-2.72,-2.93,-3,-2.29,-1.95,-.04))
plot(x)
ar(x,aic=FALSE,order.max=1,demean=FALSE,method = "yule-walker")
arima(x,order=c(1,0,0),method="CSS", include.mean = FALSE)
