library(data.table)
library(dplyr)
require(stats)

setwd("C:/Coursera/R/2FProgAssignment3data")

rankall <- function (outcome, num = "best") {
  # num = "worst"
  # outcome="pneumonia"
  # outcome = "heart attack"
  # num = 20

  
  df1 <- data.frame(read.csv("outcome-of-care-measures.csv",header=TRUE, colClasses="character",na.strings = c("NA", "Not Available") ))
  
  # Prepare some lists
  deseases <- c("heart attack", "heart failure", "pneumonia")
  states <- as.list(unique(df1[,"State"]))
  hospitals <- unique(df1[,"Hospital.Name"])

  # sanity checks on input and dataframe creation
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
    print("Pneumonia")
    df2 <- select(df1, 2, 7, 23)
  }
  
  df2[,3] <- as.numeric(df2[,3])
  
  # order hospital by states
  for (state in as.list(states)){
    df3 <- df2[order(df2[,2]),]
  }
  
  # order hospital by states and by score
  for (state in as.list(states)){
    df4 <- df3[order(df2[,3]),]
  }
  
  # create output frames
  output <- data.frame()
  tmp_frame <- data.frame()
 
  # state = "WI"
  for (state in as.list(states)){
            print(state)
            # create selection on state
            selection <- df4[df4$State == state,]
            # order selection
            df5 <- selection[order(selection[,3]),]
            
            # make copy - troubleshooting purposes          
            df6 <- df5
            
            
            # if num = best / worst
            if ( num == "best" ) {
              ties = min(df6[,3], na.rm=TRUE) 
              # set row number to row where min/max is found 
              num = which(df6[,3] == ties)
              tmp_frame <- data.frame(state, df6[num[1],1], state ) # write min
              names(tmp_frame) <- c(" ","hospital", "state")
              #output <- rbind(output, tmp_frame)
            } # best is a minimum, 'ties' is the min/max value of that row
            if ( num == "worst") {
              ties = max(df6[,3], na.rm=TRUE)  
              num = which(df6[,3] == ties)
              tmp_frame <- data.frame(state, df6[num[1],1], state ) # write max
              #names(tmp_frame) <- c(" ","hospital", "state")
              #output <- rbind(output, tmp_frame)
            }
              
            # if num exceeds frame size
            if ( num > nrow(df6)) {
              tmp_frame <- data.frame(state, "<NA>", state ) # write NA
              #output <- rbind(output, tmp_frame)
            }
            # if rownumber exists in frame
            if ( num <= nrow(df6)) {
              ties = df6[num, 3]
              if (!is.na(ties)  == TRUE) { # check numeric?
                  selected_ties <- df6[which(df6[,3] == ties),]
                  selected_ties <- selected_ties[order(selected_ties[,1]),]
                  selection <- selected_ties[1,] # take first row
                  tmp_frame <- data.frame(state, selection[1,1],state )
                  #names(tmp_frame) <- c(" ","hospital", "state")
                  
              } else {
                    tmp_frame <- data.frame(state, "<NA>", state ) # something went wrong
                    #names(tmp_frame) <- c(" ","hospital", "state")
                    }
            }
    names(tmp_frame) <- c(" ","hospital", "state")
    output <- rbind(output, tmp_frame)           
    names(output) <- c(" ","hospital", "state")
    
   } # end-for
  
  #names(tmp_frame) <- c(" ","hospital", "state")
  output[order(output[3]),]

} #end-rankall


## TESTS


head(rankall("heart attack", 20),10)

# hospital state
# 2  AK                                <NA>    AK
# 1  AL      D W MCMILLAN MEMORIAL HOSPITAL    AL
# 4  AR   ARKANSAS METHODIST MEDICAL CENTER    AR
# 3  AZ JOHN C LINCOLN DEER VALLEY HOSPITAL    AZ
# 5  CA      ENCINO HOSPITAL MEDICAL CENTER    CA
# 6  CO            SKY RIDGE MEDICAL CENTER    CO
# 7  CT             MIDSTATE MEDICAL CENTER    CT
# 9  DC                                <NA>    DC
# 8  DE                                <NA>    DE
# 10 FL       BAPTIST HOSPITAL OF MIAMI INC    FL

# has a small problem, works for individual state
# but putting the rows together, using for loop has appearently a problem
tail(rankall("pneumonia", "worst"),3)

tail(rankall("heart failure"), 10)

# hospital state
# 44 TN                         WELLMONT HAWKINS COUNTY MEMORIAL HOSPITAL    TN
# 45 TX                                        FORT DUNCAN MEDICAL CENTER    TX
# 46 UT VA SALT LAKE CITY HEALTHCARE - GEORGE E. WAHLEN VA MEDICAL CENTER    UT
# 49 VA                                          SENTARA POTOMAC HOSPITAL    VA
# 48 VI                            GOV JUAN F LUIS HOSPITAL & MEDICAL CTR    VI
# 47 VT                                              SPRINGFIELD HOSPITAL    VT
# 50 WA                                         HARBORVIEW MEDICAL CENTER    WA
# 52 WI                                    AURORA ST LUKES MEDICAL CENTER    WI
# 51 WV                                         FAIRMONT GENERAL HOSPITAL    WV
# 53 WY                                        CHEYENNE VA MEDICAL CENTER    WY


# r <- rankall("heart attack", 4)
# as.character(subset(r, state == "HI")$hospital)

# r <- rankall("heart failure", 10)
# as.character(subset(r, state == "NV")$hospital)


# r <- rankall("pneumonia", "worst")
# as.character(subset(r, state == "NJ")$hospital)


