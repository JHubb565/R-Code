################################################################################
Justin Hubbard

################################################################################

# load in data
getwd()
setwd("C:\\Users\\malan\\Dropbox\\_Purdue\\_Teaching\\DM\\6_Dimension Reduction")

# Book Example - p.96 1990 California census housing data
# Each record corresponds to a census block - each block has on avg 1425 people
# Y = "median_house_value" : income in thousands of dollars
# X1 = "median_income"      : income scaled between 0 and 15
# X2 ="housing_median_age" : age in years of house
# X3 ="total_rooms"        : # of rooms in block
# X4 ="total_bedrooms"     : # bedrooms in block
# X5 ="population"         : # of people living in block
# X6 ="households"         : # of households in block
# X7 ="latitude"           : center latitute of block
# X8 ="longitude"          : center longitude of block
myUrl <- "https://raw.githubusercontent.com/MatthewALanham/Datasets/master/1990CAHousingData.csv"
df <- read.table(file=myUrl, header=T, sep=",")
str(df)
head(df, n=2)
names(df)
names(df) <- c("value","income","homeage","rooms","beds","pop","homes","lat","long")

################################################################################
# summarize features
library(psych)
stats <- describe(df)

# distribution of each variable
par(mfrow=c(3,3))
for (i in 1:9) {
    hist(df[,i], xlab=names(df)[i], main=names(df)[i], col="blue", prob=T)
    lines(density(df[,i]), col="red", lwd=2) #adds density line
}

# distribution and correlation of specific variables
library(GGally)
ggpairs(df[,2:9])

# variance-covariance matrix
corMatrix <- cor(df)
corMatrix 

# Looking at the correlation among the numeric features
library(corrgram)
corrgram(df
         , order=F
         , lower.panel=panel.shade
         , upper.panel=panel.shade
         , text.panel=panel.txt
         , main="Correlogram of California House Data"
         )

# ignore Y response variable
df <- df[,2:9]

################################################################################
# easy way to standardize variables
dfz <- data.frame(scale(df))

# change variable names so they have a "_z" after them
for (i in 1:8) {
    names(dfz)[i] <- paste0(names(dfz)[i],"_z")
}
names(dfz)
    
# check that we get mean of 0 and sd of 1
summary(dfz)       # faster version of apply(scaled.dat, 2, mean)
apply(dfz, 2, sd)  # standard deviations for each standardized variable

# distribution of each variable
par(mfrow=c(3,3))
for (i in 1:8) {
    hist(dfz[,i], xlab=names(dfz)[i], main=names(dfz)[i], col="blue", prob=T)
    lines(density(dfz[,i]), col="red", lwd=2) #adds density line
}

# distribution and correlation of specific variables
library(GGally)
ggpairs(dfz[,1:8])

# variance-covariance matrix
corMatrixz <- cor(dfz)
corMatrixz 

# Looking at the correlation among the numeric features
library(corrgram)
corrgram(dfz
         , order=F
         , lower.panel=panel.shade
         , upper.panel=panel.shade
         , text.panel=panel.txt
         , main="Correlogram of California House Data"
)

################################################################################
# randomly choose a training and testing set
set.seed(1234) # use this to replicate results
choose <- runif(nrow(dfz))
train <- dfz[which(choose > 0.1),]  # this is the data set we do PCA on
test <- dfz[which(choose <= 0.1),]  # this is our evaluation set (like new data)

# pca using the psych package
# be aware that if you get an error its likely due to multicollinearity
library(psych)
names(train)
pca1 <- principal(train
                  , nfactors = 8     # number of componets to extract
                  , rotate = "none"  # can specify different rotations
                  , scores = T       # find component scores or not
)

# eignvalues
pca1$values

# loadings matrix, variance explained
# Cumulative Var is the proportion of total variability explained in Z that is
# explained by the cumulative of ith principal components
pca1$loadings

# Here we see that the first component accounts for nearly half (49.0%) of
# the total variance in the dataset.
#                 PC1   PC2   PC3   PC4   PC5   PC6   PC7   PC8
#SS loadings    3.922 1.909 1.073 0.818 0.137 0.081 0.047 0.014
#Proportion Var 0.490 0.239 0.134 0.102 0.017 0.010 0.006 0.002
#Cumulative Var 0.490 0.729 0.863 0.965 0.982 0.992 0.998 1.000

# the sum of the eigenvalues always equals the total number of m features
sum(pca1[[1]])

# Using the eigenvalue criterion - how many components to keep?
length(pca1[[1]][pca1[[1]] > 1])

# scree plot
par(mfrow=c(1,1))
plot(pca1$values, type="b", main="Scree plot"
     , col="blue", xlab="Component #", ylab="Eigenvalues", pch=19)

# plot factor scores from 3rd PC
pairs(cbind(train[,c("income_z", "homeage_z")], pca1$scores[,3])
      , labels = c("Median income","Home Age","PC 3 Scores"))
# plot factor scores from 4th PC
pairs(cbind(train[,c("income_z", "homeage_z")], pca1$scores[,4])
      , labels = c("Median income","Home Age","PC 4 Scores"))

# calculate communalities
com3 <- loadings(pca1)[2,1]^2 + loadings(pca1)[2,2]^2 + loadings(pca1)[2,3]^2
com4 <- loadings(pca1)[2,1]^2 + loadings(pca1)[2,2]^2 + loadings(pca1)[2,3]^2 +
        loadings(pca1)[2,4]^2
com3; com4

# validation of pca
pca2 <- principal(test
                  , nfactors = 4     # number of componets to extract
                  , rotate = "none"  # can specify different rotations
                  , scores = T       # find component scores or not
)
pca2$loadings
