channel=odbcConnect(dsn = "PostgreSQL35W", uid="postgres",pwd="*****")
odbcSetAutoCommit(channel,autoCommit=TRUE)

install.packages("RODBC", "sqldf", "SASxport", "sas7bat")
library(RODBC) #connect to postgres
library(sqldf) #install sql for R data
library(SASxport) #install SAS export
library(sas7bdat) #install SAS for R

LIBRARY = ""           #library location
FILE = ""              #file name and extension
SAS_FILE_NAME = ""     #SAS FILE NAME

setwd(LIBRARY) #set library
data = read.table(FILE, header=TRUE, sep = ",") #read in data via file csv
data = read.table(FILE, header=TRUE, sep = "|") #read in data via file pipe

data = sqlQuery(channel, "Select *
                            from SCHEMA.TABLE_NAME") #initial query of the data and assignment

#Look at data and examine beginning relationships

F1 = function(data, sas_file) {
    if (dim(data)[1] > 0) {
      print(summary(data)) #summary
      print(colnames(data)) #get col names
      print(sapply(data, function(data) sum(is.na(data)))) #count number of missing per column
      #write.xport(data, file = sas_file) #Export to SAS for SQL/EMINER
    } else {
      print("Error data does not exist")
    }
}

F1(data = data, sas_file = SAS_FILE_NAME )


#' A Correlation Matrix Function
#'
#' This function allows you to construct a correlation Matrix
#' @param Data, lower, upper, cut_off
#' @keywords Correlation matrix, variable
#' @export
#' @examples
#' x=rnorm(100,5)
#' y=rnorm(100,5)
#' z=rnorm(100,5)
#' d=data.frame(x,y,z)
#' DMcorMatrix(data=d, lower=1, upper=3, cut_off=0)


DMcorMatrix <-
  function(data, lower, upper, cut_off){
    if (dim(data)[1] > 0) {
      corMatrix = cor(data[,c(lower:upper)])
      #loop through correlation matrix
      for (i in 1:dim(corMatrix)[1]) {
        for (j in 1:dim(corMatrix)[2]){
          if ((abs(corMatrix[i,j]) < cut_off | i==j)&(cut_off!=0)){
            corMatrix[i,j]=NA
          } else if ((abs(corMatrix[i,j]) < cut_off | i==j)&(cut_off!=0)) {
            corMatrix[i,j] = corMatrix[i,j]
          }
        }
      }
      corMatrix <- corMatrix[, colSums(is.na(corMatrix)) < dim(corMatrix)[1]]
      corMatrix <- corMatrix[rowSums(is.na(corMatrix)) < dim(corMatrix)[2],]
      corMatrix
    }
  }


DMcorMatrix(data=d, lower=1, upper=3, cut_off=0)


