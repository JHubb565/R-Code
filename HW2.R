#HW2 MGMT490
#JUSTIN HUBBARD

#Q1 
install.packages(c("ISLR","scales","mice","reshape","GGally","ggplot2"))
library(ISLR)
library(scales)
library(mice)
library(reshape)
library(GGally)
library(ggplot2)

#Q2
data(Hitters) #loading in data
dim(Hitters)

#Q3
str(Hitters)

#Q4
summary(Hitters) #Summary of Hitters Data
IQR(Hitters$Salary, na.rm=TRUE) #IQR Hitters Salary
table(Hitters$Division) #count E/W division variable

#Q5
v=names(Hitters)
f1=function(data_set)
{
  data(data_set)
  nam=names(data_set)
    for (i in 1:ncol(data_set)){
      fullnam=data_set[[paste0(i)]]
      #fullnam=paste0(data_set$i)
      mean_fullnam = mean(fullnam)
      return(fullnam)
#       median_fullnam=median(fullnam)
#     }
#   if (median_fullnam > mean_fullnam)
#     print(fullnam)
#   else 
#     print("No variables")
    }}

f1(data_set = Hitters)



salaryRaw<-Hitters$Salary
Hitters$mean_sal<-mean(Hitters$Salary,na.rm=TRUE)
salaryImpMean<-Hitters$Salary
salaryImpMean[is.na(Hitters$Salary)]<-Hitters$mean_sal

