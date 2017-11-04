
# T. Broekhof


library(stats)
library(tools)
library(dplyr)
library(data.table)



complete <- function(directory, id = 1:322) {
  
  wd <- dirname(normalizePath(directory))
  setwd(wd)

  result <- data.frame("ID"= NULL, "nob" = NULL )
  for (i in id) {
    df  <- rbindlist(lapply(sprintf("%03d.csv", i), fread))
    test <- df[complete.cases(df)]
    result <- rbind(result, c(i, nrow(test)))
    names(result) <- c("ID", "nob")
   
  }
  print(result)
 

}

# Run some tests
complete("specdata", 1)

complete("specdata", c(2, 4, 8, 10, 12))

complete("specdata", 30:25)

complete("specdata", 3)

