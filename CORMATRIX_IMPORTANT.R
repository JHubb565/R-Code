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

?cor
