################################################################################
### Programming Assignment 4
################################################################################

#2)
best <- function(state, outcome) {
  #Turn warnings off while function executes; without this you get a warning
  #about coersing NA's when we make the dataframe numeric.
  oldwarn <- getOption("warn")
  options(warn = -1)

  #Check to see if data frame already exists & create if not: 
  if (!exists("outcomedf", where = ".GlobalEnv")) {
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

  #Return the best heart attack outcome: 
  if (outcome == "heart attack") {
      stateHeartAttacks <- subset(outcomedf, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      lowHeartAttack <- subset(stateHeartAttacks, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == min(stateHeartAttacks[,2], na.rm = TRUE))
      return(lowHeartAttack[, 1])
  }
  
  #Return the best heart failure outcome: 
  if (outcome == "heart failure") {
    stateHeartFailures <- subset(outcomedf, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
    lowHeartFailure <- subset(stateHeartFailures, Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == min(stateHeartFailures[,2], na.rm = TRUE))
    return(lowHeartFailure[, 1])
  }
  
  #Return the best pneumonia outcome: 
  if (outcome == "pneumonia") {
    statePneumonia <- subset(outcomedf, State == state, select = c(Hospital.Name, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
    lowPneumonia <- subset(statePneumonia, Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == min(statePneumonia[,2], na.rm = TRUE))
    return(lowPneumonia[, 1])
  }
  
  #Reset to old warning level: 
  options(warn = oldwarn)
}