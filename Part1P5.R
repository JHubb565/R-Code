#PART 1 PROBLEM 5
#Xt = ARIMA(p, 1, q) process with ??1 = ???0.6, ??2 = 0.2, ??1 = 1,
#and ??2 = ???1 then plot the autocorrelation and partial autocorrelation functions.
arima.sim(n = 500, list(ar = c(-.6,.2), ma = c(1, -1)))
x=arima.sim(n = 500, list(ar = c(-.6,.2), ma = c(1, -1)))
acf2(x)
#Define
#Yt = ???Xt
y=diff(x,lag=1,differences = 1)
summary(y)
plot.ts(y,main="Justin Hubbard Part 1 Problem 5 First Difference")
acf2(y)

plot(1,main="Justin Hubbard Part 1 Problem 3 Brownian Motion 2D")
