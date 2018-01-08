#2.2
Time=(c(1:16))
x=(c(1.6, 0.8, 1.2, 0.5, 0.9, 1.1, 1.1, 0.6, 1.5, 0.8, 0.9, 1.2, 0.5, 1.3, 0.8, 1.2))
plot.ts(Time,x,main="Justin Hubbard 2.2")
mean(x)
var(x)
acf(x,main="Justin Hubbard 2.2")
library("astsa")
acf2(x,main="Justin Hubbard 2.2")

#2.4
r=(c(.02,.05,-.09,-.08,-.02,0,.12,-.06,.02,-.08))
plot(r, main="Justin Hubbard 2.4, r value plot")

