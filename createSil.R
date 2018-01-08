################################################################################
# Justin Hubbard
################################################################################

createSil <- function(clusters, distMatrix) {
    
library(cluster)  
#km3 <- kmeans(dfz, 3)
#distMatrix <- dist(dfz, method="euclidean")
#clusters <- km3$cluster

# generate silhoutte
mySil <- silhouette(clusters, distMatrix)

# capture stats
avgSilWidth <- round(summary(mySil)$avg.width,2)
cAvg <- round(matrix(summary(mySil)$clus.avg.widths),2)
cSizes <- matrix(summary(mySil)$clus.sizes)

# how many clusters are there?
n <- dim(table(clusters))

# create silhouette plot
mySil <- mySil[order(mySil[,"cluster"], -mySil[,"sil_width"]),] 

#par(mfrow=c(1,1))
plot(mySil[which(mySil[,"cluster"]=="1"),"sil_width"], col="blue", pch=19
     , type="b", ylim=c(0,max(mySil[,"sil_width"])), ylab="Silhouette values"
     , cex.lab=1, xlab="Points in each cluster"
     , xlim=c(0,max(table(mySil[,"cluster"])))
     , main=paste0("Silhouette"," [Avg Sil=",avgSilWidth,"]"))
if (n==2){
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2])))
} else if (n==3){
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3])))
} else if (n==4){    
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="4"),"sil_width"], col="orange", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green","orange"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3]),
                      paste0("n=",cSizes[4]," ","Avg=",cAvg[4])))
} else if (n==5){    
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="4"),"sil_width"], col="orange", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="5"),"sil_width"], col="purple", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green","orange","purple"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3]),
                      paste0("n=",cSizes[4]," ","Avg=",cAvg[4]),
                      paste0("n=",cSizes[5]," ","Avg=",cAvg[5])))
} else if (n==6){    
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="4"),"sil_width"], col="orange", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="5"),"sil_width"], col="purple", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="6"),"sil_width"], col="brown", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green","orange","purple","brown"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3]),
                      paste0("n=",cSizes[4]," ","Avg=",cAvg[4]),
                      paste0("n=",cSizes[5]," ","Avg=",cAvg[5]),
                      paste0("n=",cSizes[6]," ","Avg=",cAvg[6])))
} else if (n==7){    
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="4"),"sil_width"], col="orange", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="5"),"sil_width"], col="purple", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="6"),"sil_width"], col="brown", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="7"),"sil_width"], col="yellow", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green","orange","purple","brown","yellow"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3]),
                      paste0("n=",cSizes[4]," ","Avg=",cAvg[4]),
                      paste0("n=",cSizes[5]," ","Avg=",cAvg[5]),
                      paste0("n=",cSizes[6]," ","Avg=",cAvg[6]),
                      paste0("n=",cSizes[7]," ","Avg=",cAvg[7])))
} else if (n==8){    
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="4"),"sil_width"], col="orange", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="5"),"sil_width"], col="purple", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="6"),"sil_width"], col="brown", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="7"),"sil_width"], col="yellow", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="8"),"sil_width"], col="black", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green","orange","purple","brown"
                               ,"yellow","black"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3]),
                      paste0("n=",cSizes[4]," ","Avg=",cAvg[4]),
                      paste0("n=",cSizes[5]," ","Avg=",cAvg[5]),
                      paste0("n=",cSizes[6]," ","Avg=",cAvg[6]),
                      paste0("n=",cSizes[7]," ","Avg=",cAvg[7]),
                      paste0("n=",cSizes[8]," ","Avg=",cAvg[8])))
} else if (n==9){    
    points(mySil[which(mySil[,"cluster"]=="2"),"sil_width"], col="red", pch=19
           , type="b")  
    points(mySil[which(mySil[,"cluster"]=="3"),"sil_width"], col="green", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="4"),"sil_width"], col="orange", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="5"),"sil_width"], col="purple", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="6"),"sil_width"], col="brown", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="7"),"sil_width"], col="yellow", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="8"),"sil_width"], col="black", pch=19
           , type="b")
    points(mySil[which(mySil[,"cluster"]=="9"),"sil_width"], col="magenta", pch=19
           , type="b")
    legend("bottomleft", col=c("blue","red","green","orange","purple","brown"
                               ,"yellow","black","magenta"), pch=19
           , legend=c(paste0("n=",cSizes[1]," ","Avg=",cAvg[1]),
                      paste0("n=",cSizes[2]," ","Avg=",cAvg[2]),
                      paste0("n=",cSizes[3]," ","Avg=",cAvg[3]),
                      paste0("n=",cSizes[4]," ","Avg=",cAvg[4]),
                      paste0("n=",cSizes[5]," ","Avg=",cAvg[5]),
                      paste0("n=",cSizes[6]," ","Avg=",cAvg[6]),
                      paste0("n=",cSizes[7]," ","Avg=",cAvg[7]),
                      paste0("n=",cSizes[8]," ","Avg=",cAvg[8]),
                      paste0("n=",cSizes[9]," ","Avg=",cAvg[9])))
} else {
}

# average silhouette values by cluster
avgSilValues <- aggregate(mySil[,"sil_width"], by=list(mySil[,"cluster"]), FUN=mean)
names(avgSilValues) <- c("Cluster","Avg Silhouette")
avgSilValues

}