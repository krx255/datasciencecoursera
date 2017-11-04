# OPTIONAL : Install package needed to use rbindlist
#
#
# install.packages("data.table")
# library(data.table)

pollutantmean <- function(directory, pollutant, id=1:322){

  # set workdir to contents to 'directory'
  wd <- dirname(normalizePath(directory))
	setwd(wd)

	
  # remove result.csv from previous run   
  if (file.exists("result.csv") ==TRUE) {file.remove("result.csv")}
  
  # expanding id to match csv files
  my_files_list <- sprintf("%03d.csv", id)
  
  # convert str to list
  # my_files <- as.list(strsplit(my_files_list," "))  
  
  # Use my_files_list to ceate df
  df  <- rbindlist(lapply(my_files_list, fread))
  
  # reject NA values
  my_subset <- subset(df,!is.na(df[[pollutant]]))
  
  # Calculate mean
  mean(my_subset[[pollutant]])
  
}  

#print("Running tests")

(R.version.string)

pollutantmean("specdata", "sulfate", 1:10) # Expected answer: 4.064128) 

pollutantmean("specdata", "nitrate", 70:72) # Expected answer: 1.706047

pollutantmean("specdata", "nitrate", 23) # Expected answer: 1.280833)




