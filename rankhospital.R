rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv")
        complist <- suppressWarnings(data.frame(Hospital = outcomedata[,2],
                                                State = outcomedata[,7],
                                                HeartAttack = as.numeric(outcomedata[,11]),
                                                HeartFailure = as.numeric(outcomedata[,17]),
                                                Pneumonia = as.numeric(outcomedata[,23])))
        
        ## Check that state and outcome are valid
        if(sum(stri_count_fixed(state,complist$State)) == 0) {
                stop("invalid state")
        }
        if(sum(stri_count_fixed(outcome,c("heart attack", "heart failure", "pneumonia"))) ==0){
                stop("invalid outcome")
        }
        
        ## check that rank is valid
        if( num != "best" && num !="worst" && class(num) != "numeric") {
                stop("invalid rank - not one of 'best', 'worst', or numeric")
        }
       
        ## created sorted data.frames based on selected outcome
        ## and number to replace num with if "worst"
        sortedHA <- subset(
                complist[order(complist$HeartAttack, complist$Hospital, na.last=NA),],
                State == state)
        lastHA <- length(sortedHA[,1])
        sortedHF <- subset(
                complist[order(complist$HeartFailure, complist$Hospital, na.last=NA),],
                State == state)
        lastHF <- length(sortedHF[,1])
        sortedP <- subset(
                complist[order(complist$Pneumonia, complist$Hospital, na.last=NA),],
                State == state)
        lastP <- length(sortedP[,1])
        
        if(num == "best")
                num <- 1
        else
                if(num == "worst") {
                        if(outcome == "heart attack") num <- lastHA
                        if(outcome == "heart failure") num <- lastHF
                        if(outcome == "pnumonia") num <- lastP
                }
       
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if(outcome == "heart attack"){
                result <- as.character(sortedHA[num,1])
        }
        if(outcome == "heart failure"){
                result <- as.character(sortedHF[num,1])
        }
        if(outcome == "pneumonia"){
                result <- as.character(sortedP[num,1])
        }
        result
}