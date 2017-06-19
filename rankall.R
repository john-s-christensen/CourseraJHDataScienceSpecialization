#  |  --------------------------------------------------------------------------|
#  |  4) Rankall using lapply 
#  |    - Redo here with lapply()
#  |    - Try some things again on tapply(). (what was the class of object I was
#  |        -feeding it? Al Warren's point on using double brackets for variable
#  |        -column name fed into lapply? Should I just make it a list before 
#  |        -sending to tapply?)
#  |    - Try using by()! It's an OO wrapper for tapply that accepts dataframes.
#  |    - Possibly redo this function as well using a for loop construct for 
#  |        -practice?
#  |    - Redo this function where I make use of dplyr & magrittr!
#  |  --------------------------------------------------------------------------|

getwd()
setwd("C:/Users/john.christensen/Box Sync/John Christensen/Data Science/Coursera Data Science Specialization/2) R Programming/Week 4 - Profiler")

rankall <- function(outcome, num = "best") {
  #Turn warnings off while function executes
  oldwarn <- getOption("warn")
  options(warn = -1)
  
  #Check for valid outcome: 
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("invalid outcome")
  }
  
  #Check for valid num: 
  if (class(num) == "character" && !(num %in% c("best", "worst"))) {
    stop("invalid num")
  }
  
  #Reading in the data:      
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  #Selecting columns, converting to numeric & changing colnames:
  outcomedf <- df[,c(2,7,11,17,23)]
  outcomedf[, 2] <- as.factor(outcomedf[, 2])
  outcomedf[, 3] <- as.numeric(outcomedf[, 3])
  outcomedf[, 4] <- as.numeric(outcomedf[, 4])
  outcomedf[, 5] <- as.numeric(outcomedf[, 5])
  colnames(outcomedf) <- c("hospital","state","heartattack","heartfailure","pneumonia")
  
  #Outcome column index variable creation & assignment: 
  if (outcome == "heart attack") {
    col_index <- 3
  } else if (outcome == "heart failure") {
      col_index <- 4
  } else
      col_index <- 5
  
  #Reduce to 3 columns - hospital, state & outcome:
  outcomedf <- outcomedf[, c(1,2,col_index)]
  
  #Order the dataset by state, outcome, hospital name & also remove NA's from outcome: 
  outcomedf <- outcomedf[order(outcomedf[,2], outcomedf[,3], outcomedf[,1], na.last = NA, decreasing = FALSE),]
  
  #Split the dataframe into a big list: 
  splitOutcomedf <- split(outcomedf[, c(1,2)], outcomedf[,2])
  
  #Create a function to be looped in lapply: 
  gatherHospitals <- function(data, num) {
    #Gather the best hospitals:
    if (num == "best") {
      #data[nrow(df) - (nrow(df) - 1),]
      return(data[1,]) #always, always, always remember to use return()!
      #data[1,]
    }
    #Gather the worst hospitals:
    if (num == "worst") {
      return(data[nrow(data),])
    } else {
    #Gather ranked hospitals:
      return(data[num,])
      }
    }
  
  #Run lapply on function gatherHospitals:
  listOfHospitals <- lapply(X = splitOutcomedf, FUN = gatherHospitals, num)
  
  #Change list back to dataframe & return:
  answerDf <- do.call(rbind.data.frame, listOfHospitals)
  return(answerDf)
  
  #Reset to old warning level: 
  options(warn = oldwarn)
}
