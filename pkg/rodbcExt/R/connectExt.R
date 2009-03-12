# Author: 	Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date: 18 February 2009
# Version 0.1.1  
# License GPL3

dsnConnect <- function(dsn, retries=3, interval=60, ...){
    cnt <- 0
    repeat {
		cnt<-cnt+1
		con <- odbcConnect(dsn, ...)
		if (con!=-1){
		    return(con)
			break
		}
		else if (cnt > retries) {
			cat("Unable to connect to database on ",dsn,". \n", sep="")
			return(NULL)
			stop();
		} else{
            Sys.sleep(interval*cnt)
        }
		rm(con)
		cat("Retrying to connect. (retries=",retries,") \n", sep="")
	}    
}

drvConnect <- function(drvname, server, db, usr, pwd, opt=27, retries=3, interval=60, ...){
    cstring <- paste("DRIVER={",drvname,"};SERVER=",server,";DATABASE=",db,";USER=",usr,";PASSWORD=",pwd,";OPTION=",opt,";", sep="")
    cnt <- 0
    repeat {
		cnt<-cnt+1
		con <- odbcDriverConnect(cstring, ...)
		if (con!=-1){
		    return(con)
			break
		}
		else if (cnt > retries) {
			cat("Unable to connect to database on ",server,". \n", sep="")
			return(NULL)
			stop();
		} else{
            Sys.sleep(interval*cnt)
        }
		rm(con)
		cat("Retrying to connect. (retries=",retries,") \n", sep="")
	}
}
