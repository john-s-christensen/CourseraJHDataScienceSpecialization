#  |  --------------------------------------------------------------------------|
#  |  3) RankHospitals
#  |  --------------------------------------------------------------------------|

getwd()
setwd("C:/Users/john.christensen/Box Sync/John Christensen/Data Science/Coursera Data Science Specialization/2) R Programming/Week 4 - Profiler")

rankhospital <- function(state, outcome, num = "best") {
  #Turn warnings off while function executes; without this you get a warning
  #about coersing NA's when we make the dataframe numeric.
  oldwarn <- getOption("warn")
  options(warn = -1)
  
  #Check to see if data frame already exists & create if not: 
  if (!exists("outcomedf", where = ".GlobalEnv")) { #this check in the global env doesn't actually make sense.
    #Reading in the data:      
    df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    #Trimming columns & converting to numeric: 
    outcomedf <- df[,c(7,2,11,17,23)]
    outcomedf[, 3] <- as.numeric(outcomedf[, 3])
    outcomedf[, 4] <- as.numeric(outcomedf[, 4])
    outcomedf[, 5] <- as.numeric(outcomedf[, 5])
  }
  
  #Check for valid state: 
  if (!(state %in% outcomedf[, 1])) {
    stop("invalid state")
  }
  #Check for valid outcome: 
  if (!(outcome %in% c("heart attack","heart failure","pneumonia"))) {
    stop("invalid outcome")
  }
  
  #Return the heart attack outcome: 
  if (outcome == "heart attack") {
    stateHeartAttacks <- subset(outcomedf, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
    orderedStateHeartAttacks <- stateHeartAttacks[order(stateHeartAttacks[,2],stateHeartAttacks[,1], na.last = NA, decreasing = FALSE),]
    
    if (num == "best") {
      return(orderedStateHeartAttacks[1, 1])
    }
    if (num == "worst") {
      worstStateHeartAttack <- subset(orderedStateHeartAttacks,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == max(orderedStateHeartAttacks[,2]))
      return(worstStateHeartAttack[,1])
    }
    if (is.numeric(num)) {
      return(orderedStateHeartAttacks[num,1])
    }
  }
  
  #Return the heart failure outcome: 
  if (outcome == "heart failure") {
    stateHeartFailures <- subset(outcomedf, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    orderedStateHeartFailures <- stateHeartFailures[order(stateHeartFailures[,2],stateHeartFailures[,1], na.last = NA, decreasing = FALSE),]
    
    if (num == "best") {
      return(orderedStateHeartFailures[1, 1])
    }
    if (num == "worst") {
      worststateHeartFailure <- subset(orderedStateHeartFailures,Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == max(orderedStateHeartFailures[,2]))
      return(worststateHeartFailure[,1])
    }
    if (is.numeric(num)) {
      return(orderedStateHeartFailures[num,1])
    }
  }
  
  #Return the pneumonia outcome: 
  if (outcome == "pneumonia") {
    statePneumonia <- subset(outcomedf, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    orderedStatePneumonia <- statePneumonia[order(statePneumonia[,2],statePneumonia[,1], na.last = NA, decreasing = FALSE),]
    
    if (num == "best") {
      return(orderedStatePneumonia[1, 1])
    }
    if (num == "worst") {
      worstStatePneumonia <- subset(orderedStatePneumonia,Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == max(orderedStatePneumonia[,2]))
      return(worstStatePneumonia[,1])
    }
    if (is.numeric(num)) {
      return(orderedStatePneumonia[num,1])
    }
  }
  
  #Reset to old warning level: 
  options(warn = oldwarn)
}
