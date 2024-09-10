library(stringi)
setdatapath <- function(directory){
        path <- paste("c:/users/mordo/documents/rprojects/", directory, "/",sep="")
}

pollutantmean <- function(directory, pollutant, id = 1:332){
        path <- paste("c:/users/mordo/documents/rprojects/", directory, "/",sep="")
        cumsum <- 0
        cumcount <- 0
        for(i in id) {
                namei<- paste(stri_pad(i,3,pad='0'),".csv",sep="")
                datai <- read.csv(paste(path,namei,sep=""))
                cumsum <- cumsum + sum(datai[pollutant][!is.na(datai[pollutant])])
                cumcount <- cumcount + length(datai[pollutant][!is.na(datai[pollutant])])
        }
        return(cumsum/cumcount)
}


        