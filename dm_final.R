################################################################################
# Justin Hubbard
################################################################################
# load in data
setwd("C://Users//Justin Hubbard//Documents//Purdue//Purdue- Senior//MGMT490//Final")
df <- read.table(file="Clothing.txt", header=T, sep=",",
                 colClasses = c(rep("factor",2), rep("numeric",3),"factor",
                                "numeric","factor", rep("numeric",32),
                                rep("factor",3), rep("numeric",5), "factor",
                                "numeric", "factor"))
str(df)
################################################################################
# Data cleanup
################################################################################
table(df$ZIP_CODE)

# remove the data point having just one digit for ZIP_CODE    
df <- df[which(df$ZIP_CODE!="0"),]

# Use the droplevels() function to remove the "0" as a factor level
?droplevels
df$ZIP_CODE <- droplevels(df$ZIP_CODE)

################################################################################
# EDA
################################################################################
# Summarize variables using describe function from psych package
library(psych)
# summary numeric features together
summary_num = describe(df[,c(split(names(df),sapply(df, function(x) 
               paste(class(x), collapse=" ")))$numeric)])

write.csv(summary_num,file = "summary_num.csv", sep = ",")


# summary factor features together
summary_fact = summary(df[,c(split(names(df),sapply(df, function(x) 
                paste(class(x), collapse=" ")))$factor)])

write.csv(summary_fact,file = "summary_fact.csv", sep = ",")

# Lets look at distribution of numeric features - first we'll break them into 
# different groups for better visuals

library(reshape)
a <- melt(df)
dim(table(a$variable)) # number of numerics
a1 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
                paste(class(x), collapse=" ")))$numeric[1:12])),]
a2 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
                paste(class(x), collapse=" ")))$numeric[13:25])),]
a3 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
                paste(class(x), collapse=" ")))$numeric[26:38])),]
a4 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
                paste(class(x), collapse=" ")))$numeric[39:42])),]

# plot histograms
library(ggplot2)
ggplot(a1, aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x", ncol=4) + geom_histogram(stat="bin") +
    labs(title = "Histogram of Numerical Variables",x="Value", y="Count") + 
    guides(fill = FALSE)

ggplot(a2, aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x", ncol=4) + geom_histogram(stat="bin") +
    labs(title = "Histogram of Numerical Variables",x="Value", y="Count") + 
    guides(fill = FALSE)

ggplot(a3, aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x", ncol=4) + geom_histogram(stat="bin") +
    labs(title = "Histogram of Numerical Variables",x="Value", y="Count") + 
    guides(fill = FALSE)

ggplot(a4, aes(x = value)) + 
    facet_wrap(~variable,scales = "free_x", ncol=4) + geom_histogram(stat="bin") +
    labs(title = "Histogram of Numerical Variables",x="Value", y="Count") + 
    guides(fill = FALSE)

# cluster type counts
ctypes <- data.frame(table(df$CLUSTYPE))
ctypes <- ctypes[order(-ctypes$Freq),]
ctypes

#proportion of responders in last years campaign

mean(df$RESPONSERATE) #mean response rate for last year

# Check for missing values
sapply(df, function(x) sum(is.na(x)))

# normalize and standardize feature together 
par(mfrow=c(1,4))
hist(df$PSWEATERS, main="% Sweaters", col="blue")
hist(log(df$PSWEATERS), main="log(% Sweaters)", col="blue")
hist(sqrt(df$PSWEATERS), main="SQRT(% Sweaters)", col="blue")
hist(scale(sqrt(df$PSWEATERS)), main="Standardize SQRT(% Sweaters)", col="blue")

# normalize and standardize using (Min-Max Normalization) together
library(clusterSim)
par(mfrow=c(1,4))
hist(df$FRE, main="# purchase visits", col="blue")
hist(log(df$FRE), main="log(# purchase visits)", col="blue")
hist(sqrt(df$FRE), main="SQRT(# purchase visits)", col="blue")
hist(data.Normalization(sqrt(df$FRE), type="n4")
     , main="Standardize SQRT(# purchase visits)", col="blue")

# normalize and standardize features together in one step
dfz <- data.frame(scale(sqrt(df[,c(split(names(df),sapply(df, function(x) 
          paste(class(x), collapse=" ")))$numeric)])))

library(DataMining)
corMatrix = DMcorMatrix(data = dfz, lower = 0, upper = 42, cut_off = .80)



# Min-Max Normalization (aka unitization with zero minimum)
library(clusterSim)
dfz <- data.frame(data.Normalization(sqrt(df[,c(split(names(df),sapply(df, function(x) 
    paste(class(x), collapse=" ")))$numeric)]),type="n4"))

# Lets look at distribution of normalized & standardized numeric features - 
# first we'll break them into different groups for better visuals
library(reshape)
a <- melt(dfz)
dim(table(a$variable)) # number of numerics
a1 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
            paste(class(x), collapse=" ")))$numeric[1:12])),]
a2 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
            paste(class(x), collapse=" ")))$numeric[13:25])),]
a3 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
            paste(class(x), collapse=" ")))$numeric[26:38])),]
a4 <- a[which(a$variable %in% c(split(names(df),sapply(df, function(x) 
            paste(class(x), collapse=" ")))$numeric[39:42])),]

# investigate relationships against the target variable 
library(ggplot2)
ggplot(dfz, aes(dfz[,1], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,2], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,3], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,4], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,5], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,6], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,7], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,8], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,9], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,10], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,11], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,12], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,13], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,14], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,15], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,16], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,17], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,18], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,19], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,20], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,21], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,22], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,23], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,24], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,25], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,26], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,27], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,28], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,29], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,30], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,31], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,32], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,33], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,34], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,35], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,36], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,37], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,38], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,39], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,40], fill = df$RESP)) + geom_density(alpha = 0.25)
ggplot(dfz, aes(dfz[,41], fill = df$RESP)) + geom_density(alpha = 0.25)

source("multiplot.R")
p1 <- ggplot(dfz, aes(dfz[,1], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[1]) ,x="Value", y="Count")
p2 <- ggplot(dfz, aes(dfz[,2], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[2]) ,x="Value", y="Count")
p29 <- ggplot(dfz, aes(dfz[,29], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[29]) ,x="Value", y="Count")
p30 <- ggplot(dfz, aes(dfz[,30], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[30]) ,x="Value", y="Count")
multiplot(p1,p2,p29,p30)

p31 <- ggplot(dfz, aes(dfz[,31], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[31]) ,x="Value", y="Count")
p33 <- ggplot(dfz, aes(dfz[,33], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[33]) ,x="Value", y="Count")
p35 <- ggplot(dfz, aes(dfz[,35], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[35]) ,x="Value", y="Count")
p41 <- ggplot(dfz, aes(dfz[,41], fill = df$RESP)) + geom_density(alpha = 0.25) + 
    labs(title=paste0(names(dfz)[41]) ,x="Value", y="Count")
multiplot(p31,p33,p35,p41)

################################################################################
# variance-covariance matrix
corMatrix <- cor(dfz)
corMatrix 
cut_off <- 0.80

# looping through the correlation matrix to identify multicollinear variables
for (i in 1:dim(corMatrix)[1]) {
    for (j in 1:dim(corMatrix)[2]) {
        if(abs(corMatrix[i,j]) < cut_off | i==j) {
            corMatrix[i,j] <- NA
        }   else{
            corMatrix[i,j] <- corMatrix[i,j]
        }
    }
}

# only show correlations that are "large" based off 
corMatrix <- corMatrix[, colSums(is.na(corMatrix)) < dim(corMatrix)[1]]
corMatrix <- corMatrix[rowSums(is.na(corMatrix)) < dim(corMatrix)[2],]
corMatrix



#Q11          

# remove MON, MAILED, RESPONDED, STYLES, CLASSES, 
drops <- c("MON","MAILED","RESPONDED","STYLES","CLASSES")
dfz <- dfz[,!(names(dfz) %in% drops)]

################################################################################



#MODELING AND EVALUATION PHASE.


# randomly choose a training and testing set
set.seed(1234)
choose <- runif(nrow(dfz))
train <- dfz[which(choose > 0.25),]
test <- dfz[which(choose <= 0.25),]

# pca using the psych package
library(psych)
names(train)
pca <- principal(train
                 , nfactors = 37     # number of componets to extract
                 , rotate = "none"  # can specify different rotations
                 , scores = T       # find component scores or not
)
pca

# scree plot
par(mfrow=c(1,1))
plot(pca$values, type="b", main="Scree plot", col="blue", pch=19)
abline(h=1.1, col="red")
scree(train)
?scree
# eignvalues
pca$values

# loadings matrix, variance explained
pca_loadings <- pca$loadings
write.table(x=pca_loadings, file="pca_loadings.dlm", sep="|", row.names=F,
            col.names=T)

# validation of pca
pca2 <- principal( test
                   , nfactors = 14     # number of componets to extract
                   , rotate = "none"  # can specify different rotations
                   , scores = T       # find component scores or not
)
pca2
pca_loadings2 <- pca2$loadings
write.table(x=pca_loadings2, file="pca_loadings_test.dlm", sep="|", row.names=F,
            col.names=T)

# Use PCA to create new component features
pca_final <- principal( dfz
                       , nfactors = 14     # number of componets to extract
                       , rotate = "none"  # can specify different rotations
                       , scores = T       # find component scores or not
)
str(pca_final)
pca_features <- data.frame(pca_final$scores)

################################################################################
# Here lets clean up our R environment by deleting objects we don't need to 
# free up memory
rm(a,a1,a2,a3,a4,corMatrix,ctypes,test,train,choose,cut_off,drops,i,j,p1,p2
   ,p29,p30,p31,p33,p35,p41,pca,pca_final,pca_loadings,pca_loadings2,pca2,multiplot)
################################################################################
# create a training and testing set for k-means clustering
# we'll just use our dfz dataset that has already been normalized/standardized
set.seed(1235)
rows = sample(1:nrow(dfz), round(nrow(dfz)*.7,0))
train = df[rows, ]   # we will use this later downstream
test = df[-rows, ]   # we will use this later downstream
trainz = dfz[rows, ]
testz = dfz[-rows, ]

## k-means clustering
set.seed(1235) # use this to replicate results
#run kmeans for diff values of k so we can identify performance by # of clusters
cost_df <- data.frame() #accumulator for cost results
for(k in 1:30){
    #allow up to 100 iterations for convergence, and do 5 random starts
    kmeans <- kmeans(x=trainz, centers=k, nstart=5, iter.max=100)
    
    #Combine cluster number and cost together, write to df
    cost_df <- rbind(cost_df, cbind(k, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")

# create an elbow plot
par(mfrow=c(1,1))
cost_df$cost <- cost_df$cost/1000
plot(cost_df, main="k-Means Elbow Plot", col="blue", pch=19, type="b"
     , xlab="Number of Clusters", ylab="MSE (in 1000s)", cex.lab=1.2)

# Generating silhouette plots another way for k-means (k=6,7,8)
library(cluster)
# k-means (k=6,7,8)
km6 <- kmeans(trainz, 6); dist6 <- dist(trainz, method="euclidean")
km7 <- kmeans(trainz, 7); dist7 <- dist(trainz, method="euclidean")
km8 <- kmeans(trainz, 8); dist8 <- dist(trainz, method="euclidean")

source("createSil.R")
par(mfrow=c(1,3))
createSil(clusters=km6$cluster, distMatrix=dist6)
createSil(clusters=km7$cluster, distMatrix=dist7)
createSil(clusters=km8$cluster, distMatrix=dist8)

################################################################################
## Evaluate the k=8 clustering solution based on a validation/test set
################################################################################

set.seed(1235)
library(flexclust)
# perform k-means on training set
km8 <- kcca(x=trainz, k=8, family=kccaFamily("kmeans"))
# perform k-means on test set
te_km8 <- kcca(x=testz, k=8, family=kccaFamily("kmeans"))

# k=8
km8_train <- data.frame(rep("train k=8",nrow(km8@"centers"))
                        ,cbind(c(1:nrow(km8@"centers")), km8@"centers"))
km8_test <- data.frame(rep("test k=8",nrow(te_km8@"centers"))
                       ,cbind(c(1:nrow(te_km8@"centers")), te_km8@"centers"))
names(km8_train)[1:2] = names(km8_test)[1:2] <- c("Dataset","Cluster")

# all results merged together - want this table to compare train and test stats
# for each model and cluster
results <- rbind(km8_train, km8_test)
results 

# save generated clusters to original dataset - we are going to evaluate this dude
train$clust8 <- km8@cluster
test$clust8 <- te_km8@cluster

# create a column showing which records are training and testing
train$partition <- "Train"
test$partition <- "Test"
newdf <- rbind(train, test)

# write out this dataset
write.table(x=newdf, file="rawdata_with_clusters2.dlm"
            ,quote=TRUE, sep="|", row.names=F, col.names=T)

################################################################################
# Here lets clean up our R environment by deleting objects we don't need to 
# free up memory
rm(cost_df, km8_test, km8_train, test, testz, train, trainz
   ,dist6, dist7, dist8, k, km6, km7, km8, kmeans, rows, te_km8, createSil, results)
################################################################################

################################################################################
## Use the clustering model kmeans (k=8) and add this a feature to the original data
set.seed(1234)
library(flexclust)
# perform k-means on entire data set
km8 <- kcca(x=dfz, k=8, family=kccaFamily("kmeans"))

# add clusters to dataset
clusters <- data.frame(km8@cluster)
names(clusters)[1] <- "clusters"

# cleanup
rm(km8, newdf)

################################################################################
# build a predictive model using decision trees using the PCA components, clusters,
# and the specified factor variables as inputs.
merged_data <- data.frame(cbind(df[,c(51,6,8,42,43,49)],pca_features, clusters))

# determine how many indicator variables are needed and create a data frame
str(df)
dummies <- data.frame(cbind(model.matrix(~merged_data$CC_CARD - 1),
                            model.matrix(~merged_data$PC_CALC20 - 1),
                            model.matrix(~merged_data$VALPHON - 1),
                            model.matrix(~merged_data$WEB - 1),
                            model.matrix(~merged_data$CLUSTYPE - 1)
))
# remove one factor level feature from every factor
dummies2 <- dummies[,c(2,3:4,6:8,10,12,13:62)]
names(dummies2)[1] <- "CC_Yes"
names(dummies2)[7] <- "VALPHON_Yes"
names(dummies2)[8] <- "WEB_Yes"

# the final dataset ready for predictive modeling
df <- data.frame(cbind(merged_data[,c(1,7:21)],dummies2))

# remove other datasets not needed going forward
rm(clusters, dfz, dummies, dummies2, merged_data, pca_features)

################################################################################
# make names for target if not already made 'X1' is the 'positive'/'1' class
levels(df$RESP) <- make.names(levels(factor(df$RESP)))
# levels of a factor are re-ordered so that the level specified by is first and 
# the others are moved down
df$RESP <- relevel(df$RESP,"X1")

# Split the data set into a training set and a test set
library(caret)
set.seed(2016)
inTrain <- createDataPartition(df$RESP   # your target variable
                               , p=.70      # % you want for training
                               , list=F)

# training data set
training <- df[inTrain,]

# test data set
test <- df[-inTrain,]

?trainControl()

# Here we specify how to train models - we are doing k-fold cross-validation
# by specifying method="cv" and we are doing it 10 times
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=10,       # k number of times to do k-fold
                     classProbs = T,  # Set to TRUE if binary class modeling
                     summaryFunction = twoClassSummary, #twoClassSummary if binary class model
                     allowParallel=T
                     )

# classification tree model
treefit1 <- train(RESP ~ .,
                  data = training,
                  method = "ctree2",
                  trControl = ctrl,
                  metric = "ROC")
plot(treefit1$finalModel)

# neural network model
myGrid <-  expand.grid(size = c(3,12)     # number of units in the hidden layer.
                       , decay = c(.02,0.10)  #parameter for weight decay. Default 0.
)
nrow(myGrid)

ANN <- train(RESP ~ .,             # model specification
              data = training,     # training set used to build model
              method = "nnet",     # type of model you want to build
              trControl = ctrl,    # how you want to learn
              tuneGrid = myGrid,   # the combination of tuning parameter to try
              maxit = 250,         # number of iterations to allow to learn
              metric = "ROC"       # performance measure
)

################################################################################
## Calculate predictive model performance - decide which model is better
# model 1 (tree) - train and test estimated probabilities and predicted classes
TrainProbs <- predict(treefit1, newdata=training, type='prob')[,1]
TrainClasses <- predict(treefit1, newdata=training)
TestProbs <- predict(treefit1, newdata=test, type='prob')[,1]
TestClasses <- predict(treefit1, newdata=test)
# model 2 (ANN) - train and test estimated probabilities and predicted classes
TrainProbs2 <- predict(ANN, newdata=training, type='prob')[,1]
TrainClasses2 <- predict(ANN, newdata=training)
TestProbs2 <- predict(ANN, newdata=test, type='prob')[,1]
TestClasses2 <- predict(ANN, newdata=test)

# model 1 (tree) - capture performance of the trained model and test set
cm <- confusionMatrix(data=TrainClasses, training$RESP)
cm
testCM <- confusionMatrix(data=TestClasses, test$RESP)
testCM
# model 2 (ANN) - capture performance of the trained model and test set
cm2 <- confusionMatrix(data=TrainClasses2, training$RESP)
cm2
testCM2 <- confusionMatrix(data=TestClasses2, test$RESP)
testCM2

# calculate ROC curves
install.packages("pROC")
library(pROC)    #to generate ROC curves and capture AUC
model1 <- roc(response = test$RESP
              , predictor = TestProbs
              # reverse the labels.
              , levels = rev(levels(test$RESP)))
model2 <- roc(response = test$RESP
              , predictor = TestProbs2
              # reverse the labels.
              , levels = rev(levels(test$RESP)))

# plot ROC curves
par(mfrow=c(1,1)) # reset graphics parameter to 1 plot
plot(model1, legacy.axes=T, col="red"
     , main="Receiver Operating Characteristic (ROC) Curve")
lines(model2, col="blue")
legend("bottomright", inset=0, title="Model", border="white", bty="n", cex=.8
       , legend=c("Tree","ANN")
       , fill=c("red","blue"))

# AUC values (closer to 1 is better)
auc(model1)
auc(model2)

