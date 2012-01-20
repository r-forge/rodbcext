# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

serialn <- function(x, width=2){    
    #TODO just use sprintf
    return(sprintf(paste("%0",width,"d",sep=""),x))
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

show.message <- function(..., eol="", sleeptime=0){
	cat(rep(" ", options("width")),"\r",sep="")
	cat(...,eol,sep="")
	flush.console()
	Sys.sleep(sleeptime)
}

force.directories <- function(path,...){
    
    if(!file.exists(path)){
        success <- dir.create(path,...)  
    } else success <- TRUE
    return(success)
}

withRetry <- function(expr, retries=10, initpause=30, failtime=10){
	tries <- 0
	success <- FALSE
	while(success==FALSE & (tries<retries | failtime>(initpause*tries))){
		items <- try(expr,silent=TRUE)
		if (class(items)=="try-error"){
			tries <- tries+1
			show.message("Timeout? trying again in ", (initpause*tries) ," secs...", eol="\n")
			Sys.sleep(initpause*tries)
		} else {
			success <- TRUE
		}		
	}
	if (!success) items <- vector()
	return(items)
}

strToChar <- function(str){
    return(unlist(strsplit(str,"")))
}

rescale <- function(x, oldmin, oldmax, newmin, newmax){
	return((x-oldmin)*(newmax-newmin)/(oldmax-oldmin) + newmin)
}
