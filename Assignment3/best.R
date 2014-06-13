best <- function(state, outcome) {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    outcomeFrame1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    possible <- c("heart attack","heart failure","pneumonia")
    if(is.null(outcome)) stop("invalid outcome")
    if(!outcome %in% possible) stop("invalid outcome")
    if(is.null(state)) stop("invalid state")
    if(!state %in% outcomeFrame1$State) stop("invalid state")
    
    outCol <- if(outcome =="heart attack"){
        11
    }else if(outcome =="heart failure"){
        17
    }else if(outcome =="pneumonia"){
        23
    }
    outcomeFrame1[, outCol] <- as.numeric(outcomeFrame1[, outCol])
    outcomeFrame <- outcomeFrame1[outcomeFrame1$State == state,]
    index <- with(outcomeFrame, order(outcomeFrame[,outCol], outcomeFrame$Hospital.Name))
    #outcomeFrame <- outcomeFrame[order(outcomeFrame[,outCol], na.last = NA),]
    outcomeFrame <- outcomeFrame[index,]    
    outcomeFrame[1,2]
}