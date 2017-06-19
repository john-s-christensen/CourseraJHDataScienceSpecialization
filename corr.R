#####
#Function 3

#What the request being made?
#To write a function that takes as input a directory and a threshold. 
#Then it wants me to take each file in that directory that meets the threshold (if) and calculate the 
#correlation between sulfate & nitrate.  
#I should then put each of these correlations into a vector. 
#Finally, I should display the vector.

#####

corr <- function(directory, threshold = 0) {
        #Create a list of files in the directory: 
            ListOfFiles <- list.files(directory, full.names = TRUE)
        #Creating an empty display vector: 
            #Here was my original problem. It wasn't numeric of length 0! Fixing this is what did the trick!
            displayvector <- vector(mode = "numeric", length = 0)
        #For loop to calculate the # of complete observations & an 'if' to put cor 
        #in display vector if meets threshold
            for (i in 1:332) { #Go through all files, one by one.
                openfile <- read.csv(ListOfFiles[i])  #I open each particular file & write to a dataframe
                completeobs <- nrow(na.omit(openfile))  #I save the number of complete rows in that file to a var
                if (completeobs > threshold) { #If complete cases exceed the threshold...
                    filecor <- cor(openfile[c("sulfate","nitrate")], use = "complete.obs")[2] #calculate cor of sulfate & nitrate, save 1 # to var
                    displayvector <- c(displayvector, filecor) #now add/append that file cor to the display vector!
                    #Trying something new: 
                    #displayvector <- c(displayvector, cor(openfile[c("sulfate","nitrate")], use = "complete.obs")[2])
                }
            }
        #Finally, display the vector!: 
            displayvector
}

corr("specdata", 400)
head(corr("specdata", 400))
#somewhere above something is off - I don't want to variable name, "filecor" in there.