pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
    myFiles <- list.files(path = directory, pattern="csv")
    #v <- numeric()
    MyData <- data.frame()
    for(i in id){
        table <- read.csv(paste(directory, "/", myFiles[i], sep =""))
        #v <- append(v,table[pollutant][!is.na(table[pollutant])])
        MyData <- rbind(MyData,table)
    }
    v <- MyData[pollutant][!is.na(MyData[pollutant])]
    mean(v)
}
