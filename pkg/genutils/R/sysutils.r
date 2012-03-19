# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

withRetry <- function(expr, retries=5, delay=60, inc.delay=TRUE){
  tries <- 0
  success <- FALSE
  while(success==FALSE & tries<retries){
    items <- try(expr,silent=TRUE)
    if (class(items)=="try-error"){
      show.message(items,appendLF=FALSE)
      tries <- tries+1
      tdelay <- ifelse(inc.delay,delay*tries,delay)
      timer.message("Retry in ", time=tdelay)
    } else {
      success <- TRUE
    }		
  }
  if (!success) items <- vector()
  return(items)
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
