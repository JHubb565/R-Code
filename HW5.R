#JUSTIN HUBBARD
#HW5
#MGMT 490

setwd("C://Users//Justin Hubbard//Documents//Purdue//Purdue- Senior//MGMT490//HW5")

CUST_TRANS = read.table("file:///C:/Users/Justin Hubbard/Documents/Purdue/Purdue- Senior/MGMT490/HW5/customer_transactions.csv", sep = "|", header = TRUE)

CUSTOMERS = read.table("file:///C:/Users/Justin Hubbard/Documents/Purdue/Purdue- Senior/MGMT490/HW5/Customers.csv", sep = "|", header = TRUE)

#forstudentuse, putting it in terms of that R script

df <- CUSTOMERS[,c("ID","Age","Income","MilestoStore")]
df <- df[order(df$Age,df$Income,df$MilestoStore),] 

set.seed(1234)
rows = sample(1:nrow(df), round(nrow(df)*.7,0))
train = df[rows, ]
test = df[-rows, ]

