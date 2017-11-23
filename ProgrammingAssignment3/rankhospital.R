library(data.table)
library(dplyr)
require(stats)

setwd("C:/Coursera/R/2FProgAssignment3data")



rankhospital <- function (state, outcome, num = "best") {
  # state = "TX"
  # outcome = "heart failure"
  # num =4
  
  df1 <- data.frame(read.csv("outcome-of-care-measures.csv",header=TRUE, colClasses="character",na.strings = c("NA", "Not Available") ))
  
  
  # Prepare some lists
  deseases <- c("heart attack", "heart failure", "pneumonia")
  states <- unique(df1[,"State"])
  hospitals <- unique(df1[,"Hospital.Name"])
  
  if (state %in% states){ # check input for state is valid
    print("Valid state code received")
  } else {
    stop("Invalid state")
  }
  if (outcome %in% deseases){
    print("Valid input for 'outcome' received")
  } else {
    stop("Invalid outcome") 
  }
  if (outcome == "heart attack"){
    print("Heart attack")
    
    df2 <- select(df1, 2, 7, 11)
  }
  if (outcome == "heart failure"){
    print("Heart failure")
    df2 <- select(df1, 2, 7, 17)
  }
  if (outcome == "pneumonia"){
    print("Heart failure")
    df2 <- select(df1, 2, 7, 23)
  }
  df2 <- na.omit(df2)
  df2[,3] <- as.numeric(df2[,3])
  


  ind1 <- which(df2[, "State"] == state)
  #ind1
  df3 <- df2[ind1, ]
  
 
  df4 <- df3[order(df3[3]),]
  #df4
  
  
  

  if (num == "best"){
    num = 1
  }  
  if (num == "worst"){ 
    num = nrow(df4)
  }  else {
    num = num
  }
  
  score <- (df4[num, 3 ])

  df4[which(df4[3] == score), ][1,1]
  
}

## Run tests

rankhospital("TX", "heart failure", 4)
#[1] "DETAR HOSPITAL NAVARRO"


rankhospital("MD", "heart attack", "worst")
#[1]] "HARFORD MEMORIAL HOSPITAL"

rankhospital("MN", "heart attack", 5000)
#[1] NA




#rankhospital("NC", "heart attack", "worst")

#rankhospital("WA", "heart attack", 7)

#rankhospital("TX", "pneumonia", 10)