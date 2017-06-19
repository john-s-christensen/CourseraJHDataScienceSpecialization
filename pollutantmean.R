###
# Function 1
###

pollutantmean <- function(directory, pollutant , id = 1:332) {
        FullFiles <- list.files(directory, full.names = TRUE) #A list of all files used later for read.csv           
        MeanDataFrame <- data.frame() #An empty data frame. 
        for(i in id) {
          MeanDataFrame <- rbind(MeanDataFrame,read.csv(FullFiles[i]))
        }
        #The above ^ populates the data frame with all the files in "specdata"
        #meansub <- subset(MeanDataFrame, id == id) #We don't need to make a subset if we get the loop right.
        
        #mean(MeanDataFrame$pollutant, na.rm = TRUE)  #<--couldn't get to work!
        #mean(MeanDataFrame[, pollutant], na.rm = TRUE) #I have no idea how this works! Why did this work & not $? It obviously didn't recognize 'pollutant' with $!
        mean(MeanDataFrame[[pollutant]], na.rm = TRUE) #Also, the double brackets works!
}
pollutantmean("specdata", "nitrate" ,023)

#Strange, I coudn't get this to work on an individual level.
mean(MeanDataFrame[, sulfate],na.rm = TRUE) 
#I wonder if this is R's way of dealing with the quotes?? I never would have gotten that without the help!

FullFiles <- list.files("specdata", full.names = TRUE)
FullFiles





## A little test: It worked as expected!
t <- 1:5
for(x in t) {
    print(t[x])
}
## Another little test: Ah, didn't work!
t <- 5:8
for(x in t) {
  print(t[x])
}




#Testing out individually: 
FullFiles <- list.files("specdata", full.names = TRUE)
MeanDataFrame <- data.frame()
for(i in 23) {
  MeanDataFrame <- rbind(MeanDataFrame,read.csv(FullFiles[i]))
}
#The above ^ populates the data frame with all the files in "specdata"
#meansub <- subset(MeanDataFrame, id == id) #We don't need to make a subset if we get the loop right.

mean(MeanDataFrame$"nitrate", na.rm = TRUE)