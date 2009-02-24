# Author: 	Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date: 18 February 2009
# Version 0.1  
# License GPL3

dsnConnect <- function(dsn, retries=3){
    cnt <- 0
    repeat {
		cnt<-cnt+1
		db <- odbcConnect(dsn)
		if (db!=-1){
		    return(db)
			break
		}
		else if (cnt > retries) {
			cat("Unable to connect to database on ",dsn,". \n", sep="")
			return(NULL)
			stop();
		}
		rm(db)
		cat("Retrying to connect. (retries=",retries,") \n", sep="")
	}    
}