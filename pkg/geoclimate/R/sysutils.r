# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

show.message <- function(..., eol=NULL){
    cat(...,eol,sep="")
    flush.console()
}

openURL <- function(urlstr, retries=1, verbose=FALSE){
    myurl <- url(urlstr)
    tries <- 1
    repeat{
        if (verbose){
            show.message(paste("Connecting to \n",urlstr, "(", retries, ")",sep=""), eol="\n")
        }
        try(open(myurl), silent=!verbose)
        if (isOpen(myurl)){
            break
        } else if (tries>retries){
            if(verbose) show.message("Connection Failed") 
            break   
        } else {
            tries <- tries + 1
        }    
    }
    return(myurl)
}

readURL <- function(urlstr, retries=1, verbose=FALSE){
    lines <- character(0)
    tries <- 1
    repeat{
        if (verbose){
            show.message(paste("Connecting to \n",urlstr, "(", retries, ")",sep=""), eol="\n")
        }
        lines <- try(readLines(urlstr), silent=!verbose)
        if (class(lines)=="try-error"){
            tries <- tries + 1
        } else {
            break
        }    
    }
    return(lines)    
}

force.directories <- function(path,...){
    
    if(!file.exists(path)){
        success <- dir.create(path,...)  
    } else success <- TRUE
    return(success)
}
