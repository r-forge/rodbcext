# Author: 	Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date: 18 February 2009
# Version 0.1.1  
# License GPL3

getODBCDriverList <- function(){
    info <- Sys.info()
    if (info["sysname"]=="Windows"){
        iDrivers <- readRegistry("SOFTWARE\\ODBC\\ODBCINST.INI\\ODBC Drivers","HLM")
        driverList <- names(iDrivers)
    }
    else if (info["sysname"]=="Linux"){
        iDrivers <- system("odbcinst -q -d", intern=TRUE)
        driverList <- sub("\\[", "",sub("\\]","",iDrivers))
    } else {
        cat("Not yet supported. \n")
        driverList <- character(0)
    }
    return(driverList)
}

getODBCDriver <- function(dbmsname){
    drivers <- getODBCDriverList()
    avail <- grep(dbmsname, drivers)
         
    if (length(avail)>0){
        # Return the first ODBC driver
        return(drivers[avail[1]])
    }
    else {
        return(character(0))
    }
}

mysqlDriver <- function(){
    return(getODBCDriver("MySQL"))
}
