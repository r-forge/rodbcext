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

mysqlConnect <- function(driver=getMySQLDriver(), hostname="localhost",user="root", pwd="", database="",option=3, ...){
    # check parameters 
    info <- Sys.info()   
    if(length(driver)==0){
        stop("No MySQL Driver specified/found")
    } else if(length(driver)>1){
        driver <- driver[1]
        warning("Only one driver is allowed. Using first in the vector.")
    }
    constring <- paste(
        ifelse(info["sysname"]=="Windows",paste("DRIVER={",driver,"};",sep=""),paste("DRIVER=",driver,";",sep="")),
        paste("SERVER=",hostname,";",sep=""), 
        ifelse(database=="","",paste("DATABASE=",database,";",sep="")),
        paste("USER=",user,";",sep=""),
        paste("PASSWORD=",pwd,";",sep=""),        
        paste("OPTION=",option,";",sep=""),
        sep=""
    )
    return(odbcDriverConnect(constring,...))
}

