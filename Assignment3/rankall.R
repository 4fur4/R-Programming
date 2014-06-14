rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    outcomeFrame <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    splitFrame <- split.data.frame(outcomeFrame, outcomeFrame$State)
    possible <- c("heart attack","heart failure","pneumonia")
    if(is.null(outcome)) stop("invalid outcome")
    if(!outcome %in% possible) stop("invalid outcome")
        
    outCol <- if(outcome =="heart attack"){
        11
    }else if(outcome =="heart failure"){
        17
    }else if(outcome =="pneumonia"){
        23
    }
    
    l <-lapply(splitFrame, rankhospitalInner, outCol=outCol, num=num)
#     
    df <- data.frame(matrix(unlist(l), nrow=length(splitFrame), byrow=T))
    names(df) <- c("hospital", "state")
    df
#      df <- data.frame(x = character(length(splitFrame)), y = character(length(splitFrame)), stringsAsFactors = FALSE)
#     for(i in 1:n){
#         df$x[i] <- i
#         df$y[i] <- toString(i)
#     }
#     df
    
}

rankhospitalInner <- function(splitFrame, outCol, num) {
    
    splitFrame[, outCol] <- as.numeric(splitFrame[, outCol])
    outRow <- if(num =="best"){
        1
    } else if(num =="worst"){
        length(splitFrame[,outCol])
    } else{
        num
    }
    index <- with(splitFrame, order(splitFrame[,outCol], splitFrame$Hospital.Name, na.last = NA))
    splitFrame <- splitFrame[index,]
    c(splitFrame[outRow,2],splitFrame$State[1])
}