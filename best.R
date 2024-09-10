best <- function(state, outcome) {
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
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        bestHA <- min(subset(complist, State == state)[,3],na.rm = TRUE)
        bestHF <- min(subset(complist, State == state)[,4],na.rm = TRUE)
        bestP <- min(subset(complist, State == state)[,5],na.rm = TRUE)
        
        if(outcome == "heart attack") {
                besthospital <- subset(complist, State == state & HeartAttack == bestHA)[,1]
        }
        if(outcome == "heart failure") {
                besthospital <- subset(complist, State == state & HeartFailure == bestHF)[,1]
        }
        if(outcome == "pneumonia") {
                besthospital <- subset(complist, State == state & Pneumonia == bestP)[,1]
        }
        besthospital
}