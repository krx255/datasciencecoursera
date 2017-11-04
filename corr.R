# T. Broekhof

library(utils)

corr <- function(directory, threshold = 0) {
  
  if (directory == "specdata") {
    setwd("c://coursera//R//specdata")
  }
  
  correlationVector = NULL

  ml <- list.files(path=".", pattern="*.csv$", all.files=TRUE )  
  my_list <- strsplit(ml, " ")
  
  for ( f in my_list){
    data <- read.csv(f, sep=",", header = TRUE )
    dt = na.omit(data) 
    if (nrow(dt) > threshold) {
      correlationVector = c(correlationVector, cor(dt[,2], dt[,3]))
    }
  }
  return (correlationVector)
}

cr <- corr("specdata", 150)
head(cr)
# [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814

summary(cr)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#-0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313 
 
cr <- corr("specdata", 5000)
summary(cr)
# Length  Class   Mode 
# 0   NULL   NULL 


cr <- corr("specdata")
summary(cr)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000 
 