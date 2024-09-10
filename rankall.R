rankall <- function(outcome, num = "best") {
        ## Check that outcome is valid
        if(outcome !="heart attack" && outcome != "heart failure" && outcome != "pneumonia"){
                stop("invalid outcome")
        }
        
        ## check that rank is valid
        if( num != "best" && num !="worst" && class(num) != "numeric") {
                stop("invalid rank - not one of 'best', 'worst', or numeric")
        }
        
        ## set working directory to .../Assignment3
        setwd("C:/Users/mordo/Documents/RProjects/Assignment3")
        ## note that this is only valid on my computer - deployment will need
        ## either replace this with a check that the data file exists in the wd
        ## or throw an error to tell the user to set the working directory to
        ## the location of the data file
        
        ## Read outcome data
        outcomedata <- read.csv("outcome-of-care-measures.csv")
        complist <- suppressWarnings(data.frame(Hospital = outcomedata[,2],
                                                State = outcomedata[,7],
                                                HeartAttack = as.numeric(outcomedata[,11]),
                                                HeartFailure = as.numeric(outcomedata[,17]),
                                                Pneumonia = as.numeric(outcomedata[,23])))
        ## created sorted data.frames based on selected outcome
        sortedHA <- subset(
                complist[order(complist$State, complist$HeartAttack,
                               complist$Hospital, na.last=NA),])
        sortedHF <- subset(
                complist[order(complist$State, complist$HeartFailure,
                               complist$Hospital, na.last=NA),])
        sortedP <- subset(
                complist[order(complist$State, complist$Pneumonia, 
                               complist$Hospital, na.last=NA),])
        
        ## create data.frame to hold results
        result <- data.frame("hospital" = NULL, "state" = NULL)
        
        ## For each state, find the hospital of the given rank
        
        ## Find best state if num == "best"
        if(num == "best"){
                if(outcome == "heart attack"){
                        for(i in unique(complist$State)) {
                                result <- rbind(result,
                                                data.frame("hospital" = subset(sortedHA, State == i)[1,1],
                                                           "state" = i))
                        }
                }
                if(outcome == "heart failure"){
                        for(i in unique(complist$State)) {
                                result <- rbind(result,
                                                data.frame("hospital" = subset(sortedHF, State == i)[1,1],
                                                           "state" = i))
                        }
                }
                if(outcome == "pneumonia"){
                        for(i in unique(complist$State)) {
                                result <- rbind(result,
                                                data.frame("hospital" = subset(sortedP, State == i)[1,1],
                                                           "state" = i))
                        }
                }
        }
        
        ## find worst if num == "worst"
        if(num == "worst") {
                if(outcome == "heart attack") {
                        for(i in unique(complist$State)) {
                                ihospital <- tail(subset(sortedHA, State == i),1)[,1]
                                result <- rbind(result,
                                                data.frame("hospital" = ihospital, "state" = i))
                        }
                }
                if(outcome == "heart failure") {
                        for(i in unique(complist$State)) {
                                ihospital <- tail(subset(sortedHF, State == i),1)[,1]
                                result <- rbind(result,
                                                data.frame("hospital" = ihospital, "state" = i))
                        }
                }
                if(outcome == "pneumonia") {
                        for(i in unique(complist$State)) {
                                ihospital <- tail(subset(sortedP, State == i),1)[,1]
                                result <- rbind(result,
                                                data.frame("hospital" = ihospital, "state" = i))
                        }
                }
        }
        
        ## For each state, find the hospital of the given numerical rank
        if(num != "best" && num != "worst") {
                statesplit <- split(complist, complist$State)
                if(outcome == "heart attack"){
                        for(i in unique(complist$State)) {
                                ihospital <- subset(sortedHA, State == i)[num,1]
                                result <- rbind(result,
                                                data.frame("hospital" = ihospital,
                                                           "state" = i))
                        }
                }
                if(outcome == "heart failure"){
                        for(i in unique(complist$State)) {
                                ihospital <- subset(sortedHF, State == i)[num,1]
                                result <- rbind(result,
                                                data.frame("hospital" = ihospital,
                                                           "state" = i))
                        }
                }
                if(outcome == "pneumonia"){
                        for(i in unique(complist$State)) {
                                ihospital <- subset(sortedP, State == i)[num, 1]
                                result <- rbind(result,
                                                data.frame("hospital" = ihospital,
                                                           "state" = i))
                        }
                }
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        result[order(result$state, na.last=NA),]
}