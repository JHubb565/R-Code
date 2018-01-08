#JUSTIN HUBBARD
#MGMT490
#HW4

setwd("C://Users//Justin Hubbard//Documents//Purdue//Purdue- Senior//MGMT490//HW4")

data = read.table("2016.csv",sep = ",", header = TRUE) #import data into R
str(data) #look at variables 
summary(data[4:11]) #only look at num variables

df = data
df = df[,c(2,4:11)] #drop countries and happiness rank from data set
names(df)

#Q7

library(DataMining)
library(ggplot2)

cormatrix = DMcorMatrix(data=df, lower = 2, upper =9, cut_off = .50) #cormatrix
cormatrix

#Q8

P1 = ggplot(df, aes(x = df$Trust_Government_Corruption, y = df$Economy_GDP_capita))
P1 + geom_point()
P2 = P1 + geom_point(aes(color=factor(df$Region)))
T1<-theme(                              
  plot.title = element_text(hjust = .5, face="bold", color = "black", size=12)
)
L1 = scale_color_discrete(name ="Regions")
P3 = P2 + T1 + L1 + labs(x="Government Corruption", 
               y = "GDP Per Capita", title = "Corruption vs GDP") 
P3

#Q9

Boxplot1 = ggplot(df, aes(x = df$Region, y = df$Happiness_Score)) + geom_boxplot()
L1_Boxplot1 = scale_color_discrete(name ="Regions")
T1_Boxplot1<-theme(                              
  plot.title = element_text(hjust = .5, face="bold", color = "black", size=12)
)

Boxplot_Labs = labs(x="Regions", 
                    y = "Happiness Score", title = "Region vs Happiness Score")
Boxplot_Final = Boxplot1 + L1_Boxplot1 + T1_Boxplot1+ Boxplot_Labs
Boxplot_Final + geom_boxplot(aes(fill=df$Region)) 

#Q10

Boxplot2 = ggplot(df, aes(x = df$Region, y = df$Health_Life_Expectancy)) + geom_boxplot()
L1_Boxplot2 = scale_color_discrete(name ="Regions")
T1_Boxplot2<-theme(                              
  plot.title = element_text(hjust = .5, face="bold", color = "black", size=12)
)

Boxplot_Labs2 = labs(x="Regions", 
                    y = "Life Expectancy", title = "Region vs Life Expectancy")
Boxplot_Final2 = Boxplot2 + L1_Boxplot2 + T1_Boxplot2 + Boxplot_Labs2
Boxplot_Final2 + geom_boxplot(aes(fill=df$Region)) 


#Q13
df_PCA = df
df_PCA = df[,c(3:9)] #drop region and happiness score from data set

dfz = data.frame(scale(df_PCA)) #scale variables
summary(dfz) #check summary statistics
apply(dfz, 2, sd) #check standard deviations


#Q14
set.seed(1234) #set seed to replicate results

choose <- runif(nrow(dfz))
train <- dfz[which(choose > 0.3),]  # this is the data set we do PCA on
test <- dfz[which(choose <= 0.3),]  # this is the data set we will test our PCA against
 
#Q15
install.packages("psych")
library(psych)

PCA_TRAIN = principal(train, nfactors = 7, rotate = "none", scores = T ) #do pca
 
PCA_TRAIN$values #EIGENVALUES

PCA_TRAIN


scree(train, factors = T, main = "Scree Plot for Train Data Set")

#Q20

Com1 = loadings(PCA_TRAIN)[2,1]^2
Com2 = loadings(PCA_TRAIN)[2,1]^2 + loadings(PCA_TRAIN)[2,2]^2 
Com3 = loadings(PCA_TRAIN)[2,1]^2 + loadings(PCA_TRAIN)[2,2]^2 + loadings(PCA_TRAIN)[2,3]^2
Com4 = loadings(PCA_TRAIN)[2,1]^2 + loadings(PCA_TRAIN)[2,2]^2 + loadings(PCA_TRAIN)[2,3]^2 + 
          loadings(PCA_TRAIN)[2,4]^2
Com5 = loadings(PCA_TRAIN)[2,1]^2 + loadings(PCA_TRAIN)[2,2]^2 + loadings(PCA_TRAIN)[2,3]^2 + 
  loadings(PCA_TRAIN)[2,4]^2 + loadings(PCA_TRAIN)[2,5]

Com1
Com2
Com3
Com4
Com5





