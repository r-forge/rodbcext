# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  20 February 2012
# Version 0.0.2
# Licence GPL v3


get.trmm <- function(wdate="1998-1-1", savepath=getwd(), rm.existing=FALSE){
    if (!require(ncdf)) stop("Package ncdf not found.")
    result <- vector()
	wdate <- as.Date(wdate)
	if(wdate < as.Date("1998-1-1")){
		show.message("Date ", wdate," is earlier than start of TRMM data. Using 1998-1-1 instead.", appendLF=TRUE)
		wdate <- as.Date("1998-1-1")
	}
	
	if (!force.directories(savepath, recursive=TRUE)){
		show.message("Error: Cannot create ", savepath, ".", appendLF=TRUE)
	} else {
	    prevday <- wdate-1
		fname <- paste("3B42_daily.",format(wdate, "%Y.%m.%d"),".6.nc", sep="") 
		src <- paste("http://disc3.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fftp%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B42_daily%2F",format(prevday, "%Y"),"%2F",format(prevday,"%j"),"%2F3B42_daily.",format(wdate, "%Y.%m.%d"),".6.bin&LABEL=3B42_daily.",format(wdate, "%Y.%m.%d"),".6.nc&SHORTNAME=TRMM_3B42_daily&SERVICE=HDF_TO_NetCDF&VERSION=1.02",sep="")
		outfile <- paste(savepath, fname, sep="/")
		if (!file.exists(outfile)){
			withRetry(download.file(src, outfile, method="internal", mode="wb"))    
		} else if (rm.existing | file.info(outfile)$size<2321368){
			file.remove(outfile)
			withRetry(download.file(src, outfile, method="internal", mode="wb"))
		}
		
		traster <- try(raster(outfile),silent=TRUE)    
		if(class(traster)!="try-error"){
			xy <- xyFromCell(traster,1:ncell(traster))
			prec <- values(traster)
			result <- cbind(xy,prec)
		} else {
			show.message(traster, appendLF=TRUE)
		}
	}
	return(result)
}

trmm.monthly <- function(month=1,year=1998, savepath=getwd(), rm.old=FALSE){
    if (!require(ncdf)) stop("Package ncdf not found.")
	
	if (!force.directories(savepath)) stop("Can not create di") 
    doy <- doyFromDate(paste(year,month,1,sep="-"))
    if (year<2007){
        fname <- paste("3B43.",substr(year,3,4),sprintf("%02d",month),"01.6",sep="")
        src <- paste("http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B43%2F", year,"%2F",sprintf("%03d",doy),"%2F",fname,".HDF&LABEL=",fname,".nc&SHORTNAME=TRMM_3B43&SERVICE=HDF_TO_NetCDF&VERSION=1.02", sep="")    
    } else {
        fname <- ifelse(year==2007, paste("3B43.",substr(year,3,4),sprintf("%02d",month),"01.6",sep=""),paste("3B43.",substr(year,3,4),sprintf("%02d",month),"01.6A",sep="")) 
        src <- paste("http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fftp%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B43%2F", year,"%2F",sprintf("%03d",doy),"%2F",fname,".HDF&LABEL=",fname,".nc&SHORTNAME=TRMM_3B43&SERVICE=HDF_TO_NetCDF&VERSION=1.02", sep="")        
    }
    
	outfile <- ifelse(outfile=="",paste(fname,".nc",sep=""), outfile)
	if (!file.exists(outfile)){
		withRetry(download.file(src, outfile, method="internal", mode="wb"))
	} else if (rm.old){
		file.remove(outfile)
		withRetry(download.file(src, outfile, method="internal", mode="wb"))
	} 
    
    if (month %in% c(1,3,5,7,8,10,12)){
        multiplier <- 24*31
    } else if (month==2){
        multiplier <- ifelse(isLeapYear(year),24*29,24*28)
    } else {
        multiplier <- 24*30
    }
	
    traster <- try(raster(outfile, varname="pcp"),silent=TRUE)
    if(class(traster)!="try-error"){
    	xy <- xyFromCell(traster,1:ncell(traster))
    	prec <- values(traster)
    	result <- cbind(xy,prec)
    } else {
        show.message(traster, appendLF=TRUE)
        result <- vector()
    }
    
	return(result)
}
