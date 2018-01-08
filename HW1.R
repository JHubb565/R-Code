x=10
y=13
install.packages(c("ggplot2","caret","elasticnet","devtools","forecast","arm"))
install.packages("arm")
remove.packages("arm")
library(caret)
library(devtools)
detach("package:caret", unload=TRUE)
detach("package:devtools", unload=TRUE)
#VECTORS QUESTION 15
X=c(1,3,5,7,9,11)
Y=c(2,4,6,8,10,12)
Z=X*Y
print(Z)
Z<15
Z>20
(count=(which(Z<15)))
(count2=(which(Z>20)))
df=data.frame(X,Y)
print(df)
#question 16
head(df, n=2)
tail(df,n=2)
names(df) #column names
print(df)[1] #print first column
#QUESTION 17
colnames(df)[1]=paste0("new_x")
colnames(df)[2]=paste0("new_y")
?remove
df$new_y=NULL
