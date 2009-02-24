# Author: 	Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date: 18 February 2009
# Version 0.1  
# License GPL3

getODBCDriverList <- function(){
    info <- Sys.info()
    if (info["sysname"]=="Windows"){
        iDrivers <- readRegistry("SOFTWARE\\ODBC\\ODBCINST.INI\\ODBC Drivers","HLM")
        driverList <- names(iDrivers)
        return(driverList)
    }
    else {
        cat("Not yet supported. \n")
        return(NULL)
    }
}

getODBCDriver <- function(dbmsname){
    info <- Sys.info()
    if (info["sysname"]=="Windows"){
        drivers <- getODBCDriverList()
        avail <- grep(dbmsname, drivers)
         
        if (length(avail)>0){
            # Return the first ODBC driver
            return(drivers[avail[1]])
        }
        else {
            return(NULL)
        }
        
    }
    else {
        cat("Not yet supported. \n")
        return(NULL)
    }
}

getMySQLDriver <- function(){
    return(getODBCDriver("MySQL"))
}
