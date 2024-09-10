library(stringi)
setdatapath <- function(directory){
        path <- paste("c:/users/mordo/documents/rprojects/", directory, "/",sep="")
}

complete <- function(directory, id = 1:332) {
        path <- paste("c:/users/mordo/documents/rprojects/", directory, "/",sep="")
        tabcounts <- data.frame(monid = id, nobs = 0)
        for(i in id) {
                namei<- paste(stri_pad(i,3,pad='0'),".csv",sep="")
                datai <- read.csv(paste(path,namei,sep=""))
                vectcom <- complete.cases(datai)
                goodcases <- datai$sulfate [vectcom]
                tabcounts$nobs [tabcounts$monid == i] <- length(goodcases)
        }
        tabcounts
}