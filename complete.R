#####
#Function 2

#What the problem is saying: 
  #We want a data frame with 2 columns: the id and # of obs.
  #To solve I think we need to loop through each file specified and then write the id & obs to the dataframe!
      #It's not just # obs, its complete cases. I think that means, row not missing any values!
#####

complete <- function(directory, id = 1:332) {
        #creating an empty dataframe: should I specify 2 columns & column names?
            dataframe2 <- data.frame()
        #Creating a list of files in the directory: 
            listOfFiles <- list.files(directory, full.names = TRUE)
        #Creating the loop to append each file name & obs to dataframe2: 
            for(i in id) {
                openfile <- read.csv(listOfFiles[i])  #I open each particular file ?into vector?
                obser <- nrow(na.omit(openfile))  #I save the number of complete rows in that file to a var
                iden <- i   #I save the file number to a variable 
                dataframe2 <- rbind(dataframe2,c(iden, obser))  #I append the file id & obs to a dataframe. 
            }
        #Displaying the finished dataframe: 
            colnames(dataframe2) <- c("id","nobs")
            dataframe2
}

complete("specdata",1) #Holy snikes! It worked!!!
#Need to name the columns in my dataframe.
complete("specdata",25:30)