corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    comRec <-complete(directory)
    ids <- comRec$id[comRec$nobs > threshold]
    myFiles <- list.files(path = directory, pattern="csv")
    corre <-c()
    for(i in ids){
        table <- read.csv(paste(directory, "/", myFiles[i], sep =""))
        compCases <- table[complete.cases(table),]
        corre <- append(corre, cor(compCases$nitrate,compCases$sulfate))
    }
    corre
}