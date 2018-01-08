install.packages("RODBC")
library(RODBC)
channel=odbcConnect(dsn = "*********", uid="********",pwd="*********")
odbcSetAutoCommit(channel,autoCommit=TRUE)

library(ISLR)
library(scales)
library(mice)
library(reshape)
library(GGally)
library(ggplot2)

#Question 1
setwd("C:\\Users\\Justin Hubbard\\Documents\\Purdue\\Purdue- Senior\\MGMT490\\Midterm")
spamD=read.table("spamD.dlm",header=TRUE,sep = "|")

#Question 2
install.packages("psych") #Download package
library(psych) #load package
dim(spamD) #find out how many variables
describe(spamD[1:57]) #only numeric/int (Leaves out "Spam" Variable)

#Question 3
summary(spamD$spam) #count spam and non-spam emails

#Question 4
sapply(spamD,function(x) sum(length((which(is.na(x)))))) #find missing values for each variable

#Question 5
#spamD2=data.frame(spamD[1:57])
corMatrix=cor(spamD[,c(1:57)])
corMatrix
cut_off=.75
#loop through correlation matrix
for (i in 1:dim(corMatrix)[1]) {
  for (j in 1:dim(corMatrix)[2]) {
    if(abs(corMatrix[i,j]) < cut_off | i==j) {
      corMatrix[i,j] = NA
    } else {
      corMatrix[i,j] = corMatrix[i,j]
    }
  }
}
corMatrix

#corrplot(corMatrix)
corMatrix = corMatrix[, colSums(is.na(corMatrix)) < dim(corMatrix)[1]]
corMatrix #correlation plot with values greater than .75
corMatrix = corMatrix[row.names(is.na(corMatrix)) < dim(corMatrix)[2],]
corMatrix #variables with values greater than .75

#install.packages("corrplot")
#library(corrplot)
#par(mfcol=c(1,1))
#corrplot(corMatrix,method="circle",order="alphabet")

#Question 6
dim(spamD)
spamD_Q6=data.frame(spamD$word.freq.email,spamD$word.freq.000,spamD$word.freq.money,
                    spamD$word.freq.credit,spamD$char.freq.dollar,spamD$spam) 
#create new dataset with those 6 variables
ggpairs(spamD_Q6) #create plot

#Question 7
p1 = ggplot(spamD,aes(spamD$word.freq.money,fill=spam))
p1 = p1 + geom_histogram(alpha=.5, aes(y=..density..), position='identity')
p1

p2 = ggplot(spamD, aes(spamD$word.freq.money, fill=spam))
p2 = p2 + geom_density(alpha = .2)
p2

p3 = ggplot(spamD, aes(spamD$word.freq.money, fill=spam))
p3 = p3 + geom_density(alpha = .2)
p3 = p3 + scale_x_continuous(limits = c(0,3))
p3

#Question 8
#write file to csv
write.csv(spamD, file="spamD2.csv")

#Question 14
spamD_Q14=data.frame(spamD$spam,spamD$word.freq.email,spamD$word.freq.000,
                     spamD$word.freq.money, spamD$word.freq.credit,
                     spamD$char.freq.dollar)
colnames(spamD_Q14)[colnames(spamD_Q14)=="spamD.spam"] <- "spam"
colnames(spamD_Q14)[colnames(spamD_Q14)=="spamD.word.freq.email"] <- "email"
colnames(spamD_Q14)[colnames(spamD_Q14)=="spamD.word.freq.000"] <- "zeros"
colnames(spamD_Q14)[colnames(spamD_Q14)=="spamD.word.freq.money"] <- "money"
colnames(spamD_Q14)[colnames(spamD_Q14)=="spamD.word.freq.credit"] <- "credit"
colnames(spamD_Q14)[colnames(spamD_Q14)=="spamD.char.freq.dollar"] <- "dollars"

write.table(spamD_Q14, file="spamD_New.dlm",sep= "|", quote = T,
            row.names = F, col.names = T)
