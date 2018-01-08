###############################################################################
# Homework #5
# Data Mining
# Justin Hubbard
################################################################################
# load in data
#setwd("C:\\Users\\Matthew A. Lanham\\Desktop\\Homework 5\\data")
setwd("C:\\Users\\malan\\Dropbox\\_Purdue\\_Teaching\\DM\\Abhishek grader\\Homework 5\\data")
#setwd("C:\\Users\\Matthew A. Lanham\\Dropbox\\_Purdue\\_Teaching\\DM\\Abhishek grader\\Homework 5\\data")
customers <- read.table(file="Customers.csv", header=T, sep="|", quote = "\"'"
                        , colClasses = c(rep("factor",2),rep("numeric",3),rep("factor",4)))
transactions <- read.table(file="customer_transactions.csv", header=T, sep="|", quote = "\"'"
                           , colClasses = c(rep("factor",11),rep("numeric",12)))

################################################################################
## As we try different clustering algorithms and use various methods to make
## a decision in how many clusters there are (silhoutte, elbow plot, pseudo-F
## , etc.), we will also perform cluster cross-validation.
################################################################################
# numeric features used for clustering 
df <- customers[,c("ID","Age","Income","MilestoStore")]
df <- df[order(df$Age,df$Income,df$MilestoStore),] 

# create a training and testing set
set.seed(1234)
rows = sample(1:nrow(df), round(nrow(df)*.7,0))
train = df[rows, ]
test = df[-rows, ]

# standardize features on training set
trainz <- data.frame(scale(train[,c(2:4)]))
# change variable names so they have a "_z" after them
for (i in 1:ncol(trainz)) {
    names(trainz)[i] <- paste0(names(trainz)[i],"_z")
}
names(trainz)

# standardize features on testing set - we will use this data set later for 
# cluster evaluation purposes
testz <- data.frame(scale(test[,c(2:4)]))
# change variable names so they have a "_z" after them
for (i in 1:ncol(testz)) {
    names(testz)[i] <- paste0(names(testz)[i],"_z")
}
names(testz)

################################################################################
## k-means clustering
################################################################################
set.seed(1234) # use this to replicate results
#run kmeans for diff values of k so we can identify performance by # of clusters
cost_df <- data.frame() #accumulator for cost results
for(k in 1:15){
    #allow up to 50 iterations for convergence, and do 5 random starts
    kmeans <- kmeans(x=trainz, centers=k, nstart=5, iter.max=50)
    
    #Combine cluster number and cost together, write to df
    cost_df <- rbind(cost_df, cbind(k, kmeans$tot.withinss))
}
names(cost_df) <- c("cluster", "cost")

# create an elbow plot
par(mfrow=c(1,1))
cost_df$cost <- cost_df$cost/1000
plot(cost_df, main="k-Means Elbow Plot", col="blue", pch=19, type="b"
     , xlab="Number of Clusters", ylab="MSE (in 1000s)", cex.lab=1.2)

################################################################################
# Generating silhouette plots another way for k-means (k=2,3,4,5,6)
library(cluster)
# k-means (k=2,3,4,5,6)
km2 <- kmeans(trainz, 2); dist2 <- dist(trainz, method="euclidean")
km3 <- kmeans(trainz, 3); dist3 <- dist(trainz, method="euclidean")
km4 <- kmeans(trainz, 4); dist4 <- dist(trainz, method="euclidean")
km5 <- kmeans(trainz, 5); dist5 <- dist(trainz, method="euclidean")
km6 <- kmeans(trainz, 6); dist6 <- dist(trainz, method="euclidean")

setwd("C:\\Users\\malan\\Dropbox\\_Purdue\\_Teaching\\DM\\Abhishek grader\\Homework 5")
source("createSil.R")
par(mfrow=c(1,3))
createSil(clusters=km2$cluster, distMatrix=dist2)
createSil(clusters=km3$cluster, distMatrix=dist3)
createSil(clusters=km4$cluster, distMatrix=dist4)
par(mfrow=c(1,3))
createSil(clusters=km5$cluster, distMatrix=dist5)
createSil(clusters=km6$cluster, distMatrix=dist6)




################################################################################
## Kohonen network/SOM clustering
################################################################################
# make input data a matrix and perform kohonen network clustering
mynames <- names(trainz)
mtz <- as.matrix(trainz); colnames(mtz) <- mynames
str(mtz)

# fit the kohonen network/SOM cluster algorithm using different number of 
# clusters (different grids), and different learning rates (a tuning parameter)
install.packages("kohonen")
library(kohonen)
som13a <- som(mtz, grid=somgrid(1,3), rlen=100, alpha=c(0.05, 0.01), radius=1)
som13b <- som(mtz, grid=somgrid(1,3), rlen=100, alpha=c(0.1, 0.01), radius=1)

som22a <- som(mtz, grid=somgrid(2,2), rlen=100, alpha=c(0.05, 0.01), radius=1)
som22b <- som(mtz, grid=somgrid(2,2), rlen=100, alpha=c(0.1, 0.01), radius=1)

som23a <- som(mtz, grid=somgrid(2,3), rlen=100, alpha=c(0.05, 0.01), radius=1)
som23b <- som(mtz, grid=somgrid(2,3), rlen=100, alpha=c(0.1, 0.01), radius=1)

som33a <- som(mtz, grid=somgrid(3,3), rlen=100, alpha=c(0.05, 0.01), radius=2)
som33b <- som(mtz, grid=somgrid(3,3), rlen=100, alpha=c(0.1, 0.01), radius=2)

# plot the counts in each cluster
par(mfrow=c(2,4))
#k=3
plot(som13a, type=c("counts"), palette.name = rainbow, main = "som13a")
plot(som13b, type=c("counts"), palette.name = rainbow, main = "som13b")
#k=4
plot(som22a, type=c("counts"), palette.name = rainbow, main = "som22a")
plot(som22b, type=c("counts"), palette.name = rainbow, main = "som22b")
#k=6
plot(som23a, type=c("counts"), palette.name = rainbow, main = "som23a")
plot(som23b, type=c("counts"), palette.name = rainbow, main = "som23b")
#k=9
plot(som33a, type=c("counts"), palette.name = rainbow, main = "som33a")
plot(som33b, type=c("counts"), palette.name = rainbow, main = "som33b")

# count of observations in each cluster
table(som13a$unit.classif)
table(som13b$unit.classif)
table(som22a$unit.classif)
table(som22b$unit.classif)
table(som23a$unit.classif)
table(som23b$unit.classif)
table(som33a$unit.classif)
table(som33b$unit.classif)

# silhouttte plots for each cluster SOM cluster solution
par(mfrow=c(1,3))
createSil(clusters=som13a$unit.classif, distMatrix=dist3)
createSil(clusters=som13b$unit.classif, distMatrix=dist3)
createSil(clusters=som22a$unit.classif, distMatrix=dist4)
par(mfrow=c(1,3))
createSil(clusters=som22b$unit.classif, distMatrix=dist4)
createSil(clusters=som23a$unit.classif, distMatrix=dist6)
createSil(clusters=som23b$unit.classif, distMatrix=dist6)
par(mfrow=c(1,3))
dist9 <- dist(trainz, method="euclidean")
createSil(clusters=som33a$unit.classif, distMatrix=dist9)
createSil(clusters=som33b$unit.classif, distMatrix=dist9)

################################################################################
## Using the pseudo-F statistic to determine best clustering solution
################################################################################
install.packages("clusterSim")
library(clusterSim)

#pseudo F-statistics for k-means and Kohonen clusters
psF2 <- index.G1(x=trainz, cl=km2$cluster) #k=2
psF3 <- index.G1(x=trainz, cl=km3$cluster) #k=3
psF4 <- index.G1(x=trainz, cl=km4$cluster) #k=4
psF5 <- index.G1(x=trainz, cl=km5$cluster) #k=5
psF6 <- index.G1(x=trainz, cl=km6$cluster) #k=6

psF3a <- index.G1(x=trainz, cl=som13a$unit.classif) #k=3
psF3b <- index.G1(x=trainz, cl=som13b$unit.classif) #k=3
psF4a <- index.G1(x=trainz, cl=som22a$unit.classif) #k=4
psF4b <- index.G1(x=trainz, cl=som22b$unit.classif) #k=4
psF6a <- index.G1(x=trainz, cl=som23a$unit.classif) #k=6
psF6b <- index.G1(x=trainz, cl=som23b$unit.classif) #k=6
psF9a <- index.G1(x=trainz, cl=som33a$unit.classif) #k=9
psF9b <- index.G1(x=trainz, cl=som33b$unit.classif) #k=9

#calculate p-values
pv_k2 <- pf(psF2, df1=2-1, df2=nrow(trainz)-2, lower.tail=F) #k=3
pv_k3 <- pf(psF3, df1=3-1, df2=nrow(trainz)-3, lower.tail=F) #k=3
pv_k4 <- pf(psF4, df1=4-1, df2=nrow(trainz)-4, lower.tail=F) #k=4
pv_k5 <- pf(psF5, df1=5-1, df2=nrow(trainz)-5, lower.tail=F) #k=5
pv_k6 <- pf(psF5, df1=6-1, df2=nrow(trainz)-6, lower.tail=F) #k=6

pv_som3a <- pf(psF3a, df1=3-1, df2=nrow(trainz)-3, lower.tail=F) #k=3
pv_som3b <- pf(psF3b, df1=3-1, df2=nrow(trainz)-3, lower.tail=F) #k=3
pv_som4a <- pf(psF4a, df1=4-1, df2=nrow(trainz)-4, lower.tail=F) #k=4
pv_som4b <- pf(psF4b, df1=4-1, df2=nrow(trainz)-4, lower.tail=F) #k=4
pv_som6a <- pf(psF6a, df1=6-1, df2=nrow(trainz)-6, lower.tail=F) #k=4
pv_som6b <- pf(psF6b, df1=6-1, df2=nrow(trainz)-6, lower.tail=F) #k=4
pv_som9a <- pf(psF9a, df1=9-1, df2=nrow(trainz)-9, lower.tail=F) #k=4
pv_som9b <- pf(psF9b, df1=9-1, df2=nrow(trainz)-9, lower.tail=F) #k=4

#whichever p-value is smallest is the best clustering solution
pvalues <- data.frame(cbind(c("km=2","km=3","km=4","km=5","km=6"
                              ,"SOMa=3","SOMb=3","SOMa=4","SOMb=4"
                              ,"SOMa=6","SOMb=6","SOMa=9","SOMb=9")
                            ,c(pv_k2, pv_k3, pv_k4, pv_k5, pv_k6
                               ,pv_som3a, pv_som3b, pv_som4a, pv_som4b
                               , pv_som6a, pv_som6b, pv_som9a, pv_som9b)))
names(pvalues) <- c("Solution","p-value")
pvalues 



# delete the R objects in your R environment that you no longer need
rm(cost_df, km2, km3, km4, km5, km6,
   dist2, dist3, dist4, dist5, dist6, dist9, i, k, kmeans, mynames, rows,
   som13a, som13b, som22a, som22b, som23a, som23b, som33a, som33b,
   psF2, psF3, psF4, psF5, psF6,
   psF3a, psF3b, psF4a, psF4b, psF6a, psF6b, psF9a, psF9b, 
   pv_k2, pv_k3, pv_k4, pv_k5, pv_k6, 
   pv_som3a, pv_som3b, pv_som4a, pv_som4b, pv_som6a, pv_som6b, pv_som9a, pv_som9b,
   pvalues, mtz, df, createSil
   )
################################################################################
## Evaluate clusters based on validation/test set
## OPTION 2 ##
################################################################################
set.seed(1234)
install.packages("flexclust")
library(flexclust)
# perform k-means on training set for k=3 and 4
km3 <- kcca(x=trainz, k=3, family=kccaFamily("kmeans"))
km4 <- kcca(x=trainz, k=4, family=kccaFamily("kmeans"))
# perform k-means on test set
te_km3 <- kcca(x=testz, k=3, family=kccaFamily("kmeans"))
te_km4 <- kcca(x=testz, k=4, family=kccaFamily("kmeans"))

# k=3
km3_train <- data.frame(rep("train k=3",nrow(km3@"centers"))
                        ,cbind(c(1:nrow(km3@"centers")), km3@"centers"))
km3_test <- data.frame(rep("test k=3",nrow(te_km3@"centers"))
                        ,cbind(c(1:nrow(te_km3@"centers")), te_km3@"centers"))
names(km3_train)[1:2] = names(km3_test)[1:2] <- c("Dataset","Cluster")
# k=4
km4_train <- data.frame(rep("train k=4",nrow(km4@"centers"))
                        ,cbind(c(1:nrow(km4@"centers")), km4@"centers"))
km4_test <- data.frame(rep("test k=4",nrow(te_km4@"centers"))
                        ,cbind(c(1:nrow(te_km4@"centers")), te_km4@"centers"))
names(km4_train)[1:2] = names(km4_test)[1:2] <- c("Dataset","Cluster")

# all results merged together - want this table to compare train and test stats
# for each model and cluster
results <- rbind(km3_train, km3_test, km4_train, km4_test)
results 

# visualize cluster centers - should make it easier to compare
source("multiplot.R")

library(ggplot2)
names(testz)
p1=ggplot(results, aes(x=Dataset, y=Age_z, fill=Cluster)) + 
    geom_bar(stat="identity", position="identity")
p2=ggplot(results, aes(x=Dataset, y=Income_z, fill=Cluster)) + 
    geom_bar(stat="identity", position="identity")
p3=ggplot(results, aes(x=Dataset, y=MilestoStore_z, fill=Cluster)) + 
    geom_bar(stat="identity", position="identity")
multiplot(p1,p2,p3)

# save generated clusters to data sets - we are going to evaluate these dudes
train$clust3 <- km3@cluster
train$clust4 <- km4@cluster
test$clust3 <- te_km3@cluster
test$clust4 <- te_km4@cluster
# create a column showing which records are training and testing
train$partition <- "Train"
test$partition <- "Test"
df <- rbind(train, test)
# merge cluster information to orginial data set
library(sqldf)
clustered_customers <- sqldf("
                            SELECT c.*, df.clust3, df.clust4, df.partition
                            FROM customers c
                            LEFT OUTER JOIN df ON c.ID = df.ID
                            ")
getwd()
# write out this dataset
write.table(x=clustered_customers, file="clustered_customers.dlm"
            ,quote=TRUE, sep="|", row.names=F, col.names=T)

# delete the R objects in your R environment that you no longer need
rm(km3_test, km3_train, km4_test, km4_train, results, test, testz
   ,train, trainz, km3, km4, p1, p2, p3, te_km3, te_km4
   ,clustered_customers, multiplot
)

################################################################################
## Use the clustering model kmeans (k=4) and add this a feature to the original data
################################################################################
# standardize features used for clustering
customers = CUSTOMERS
transactions = CUST_TRANS

dfz <- data.frame(customers$"ID",scale(customers[,c("Age","Income","MilestoStore")]))
names(dfz) <- c("ID","Age_z","Income_z","MilestoStore_z")

km4 <- kcca(x=dfz[,2:4], k=4, family=kccaFamily("kmeans"))
customers$cluster <- km4@cluster
rm(km4, dfz)

################################################################################
## build a predictive model using decision trees with and without clusters
################################################################################
# merge datasets using SQL
library(sqldf)
df <- sqldf("
            SELECT c.*, t.* 
            FROM customers c
            LEFT OUTER JOIN transactions t ON c.ID = t.ID
            ")

# remove ID column (we do this twice because it shows up twice)
df$ID <- NULL
df$ID <- NULL

# make names for target if not already made 'X1' is the 'positive'/'1' class
levels(df$Alcohol) <- make.names(levels(factor(df$Alcohol)))
# levels of a factor are re-ordered so that the level specified by is first and 
# the others are moved down
df$Alcohol <- relevel(df$Alcohol,"X1")

# Split the data set into a training set and a test set
library(caret)
set.seed(2016)
inTrain <- createDataPartition(df$Alcohol   # your target variable
                               , p=.70      # % you want for training
                               , list=F)
################################################################################
## Model 1 - without clusters
################################################################################
train1 <- df[inTrain,c("Alcohol","Gender","SportsFan","Age","Income","MilestoStore")]
test1 <- df[-inTrain,c("Alcohol","Gender","SportsFan","Age","Income","MilestoStore")]

# Here we specify how to train models - we are doing k-fold cross-validation
# by specifying method="cv" and we are doing it 10 times
ctrl <- trainControl(method="cv",     # cross-validation set approach to use
                     number=10,        # k number of times to do k-fold
                     classProbs = T, # Set to TRUE if binary class modeling
                     summaryFunction = twoClassSummary #twoClassSummary if binary class model
                     )
treefit1 <- train(Alcohol ~ .,
                 data = train1,
                 method = "ctree2",
                 trControl = ctrl,
                 metric = "ROC")
plot(treefit1$finalModel)
################################################################################
## Model 2 - with clusters
################################################################################
names(df)
train2 <- df[inTrain,c("Alcohol","Gender","SportsFan","Age","Income","MilestoStore","cluster")]
test2 <- df[-inTrain,c("Alcohol","Gender","SportsFan","Age","Income","MilestoStore","cluster")]

treefit2 <- train(Alcohol ~ .,
                  data = train2,
                  method = "ctree2",
                  trControl = ctrl,
                  metric = "ROC")
plot(treefit2$finalModel)
################################################################################
## Model 3 - with only clusters
################################################################################
train3 <- df[inTrain,c("Alcohol","cluster")]
test3 <- df[-inTrain,c("Alcohol","cluster")]

treefit3 <- train(Alcohol ~ .,
                  data = train3,
                  method = "ctree2",
                  trControl = ctrl,
                  metric = "ROC")
plot(treefit3$finalModel)

################################################################################
## Calculate predictive model performance - decide which model is better
################################################################################

# model 1 - train and test estimated probabilities and predicted classes
TrainProbs <- predict(treefit1, newdata=train1, type='prob')[,1]
TrainClasses <- predict(treefit1, newdata=train1)
TestProbs <- predict(treefit1, newdata=test1, type='prob')[,1]
TestClasses <- predict(treefit1, newdata=test1)
# model 2 - train and test estimated probabilities and predicted classes
TrainProbs2 <- predict(treefit2, newdata=train2, type='prob')[,1]
TrainClasses2 <- predict(treefit2, newdata=train2)
TestProbs2 <- predict(treefit2, newdata=test2, type='prob')[,1]
TestClasses2 <- predict(treefit2, newdata=test2)
# model 3 - train and test estimated probabilities and predicted classes
TrainProbs3 <- predict(treefit3, newdata=train3, type='prob')[,1]
TrainClasses3 <- predict(treefit3, newdata=train3)
TestProbs3 <- predict(treefit3, newdata=test3, type='prob')[,1]
TestClasses3 <- predict(treefit3, newdata=test3)

# model 1 - capture performance of the trained model and test set
cm <- confusionMatrix(data=TrainClasses, train1$Alcohol)
cm
testCM <- confusionMatrix(data=TestClasses, test1$Alcohol)
testCM
# model 2 - capture performance of the trained model and test set
cm2 <- confusionMatrix(data=TrainClasses2, train2$Alcohol)
cm2
testCM2 <- confusionMatrix(data=TestClasses2, test2$Alcohol)
testCM2
# model 3 - capture performance of the trained model and test set
cm3 <- confusionMatrix(data=TrainClasses3, train3$Alcohol)
cm3
testCM3 <- confusionMatrix(data=TestClasses3, test3$Alcohol)
testCM3

# calculate ROC curves
library(pROC)    #to generate ROC curves and capture AUC
model1 <- roc(response = test1$Alcohol
                , predictor = TestProbs
                # reverse the labels.
                , levels = rev(levels(test1$Alcohol)))
model2 <- roc(response = test2$Alcohol
                 , predictor = TestProbs2
                 # reverse the labels.
                 , levels = rev(levels(test2$Alcohol)))
model3 <- roc(response = test3$Alcohol
              , predictor = TestProbs3
              # reverse the labels.
              , levels = rev(levels(test3$Alcohol)))

# plot ROC curves
par(mfrow=c(1,1)) # reset graphics parameter to 1 plot
plot(model1, legacy.axes=T, col="red"
     , main="Receiver Operating Characteristic (ROC) Curve")
lines(model2, col="blue")
lines(model3, col="green")
legend("bottomright", inset=0, title="Model", border="white", bty="n", cex=.8
       , legend=c("Model1","Model2","Model3")
       , fill=c("red","blue","green"))

# AUC values (closer to 1 is better)
auc(model1)
auc(model2)
auc(model3)