> house <- read.table("~/Purdue/Purdue- Senior/STAT 420/house.txt", quote="\"")
>   View(house)
> sales <- as.vector(as.matrix(t(house)))
> plot(sales, type='l', col='green', lwd=2)
> plot(sales, type='l', col='green', lwd=2, main="House Sales over Time")
> points(sales, col='blue')
> acf(house)
Error in plot(house) : object 'house' not found
> sales <- as.vector(as.matrix(t(file)))
Error in t.default(file) : argument is not a matrix
> 
  > 
  > house <- read.table("~/Purdue/Purdue- Senior/STAT 420/house.txt", quote="\"")
>   View(house)
> sales <- as.vector(as.matrix(t(house)))
> plot(sales, type='l', col='green', lwd=2)
> plot(sales, type='l', col='green', lwd=2, main="House Sales over Time")
> points(sales, col='blue')
> acf(house)
Error in acf(house) : 'lag.max' must be at least 0
> acf(x=sales)
> acf(x=sales, lag.max=10)
> help("acf")
> plot(house)
Error in plot(house) : object 'house' not found
> sales <- as.vector(as.matrix(t(file)))
