# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  20 January 2012
# Version 0.0.1
# Licence GPL v3

#reading hdf

downloadTRMM.monthly <- function(month=1,year=1998, outfile="", var="pcp"){
    if (!require(ncdf)) stop("Package ncdf not found.")
    doy <- doyFromDate(paste(year,month,1,sep="-"))
    if (year<2007){
        fname <- paste("3B43.",substr(year,3,4),serialn(month,width=2),"01.6",sep="")
        src <- paste("http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B43%2F", year,"%2F",serialn(doy, width=3),"%2F",fname,".HDF&LABEL=",fname,".nc&SHORTNAME=TRMM_3B43&SERVICE=HDF_TO_NetCDF&VERSION=1.02", sep="")    
    } else {
        fname <- ifelse(year==2007, paste("3B43.",substr(year,3,4),serialn(month,width=2),"01.6",sep=""),paste("3B43.",substr(year,3,4),serialn(month,width=2),"01.6A",sep="")) 
        src <- paste("http://disc2.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fftp%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B43%2F", year,"%2F",serialn(doy, width=3),"%2F",fname,".HDF&LABEL=",fname,".nc&SHORTNAME=TRMM_3B43&SERVICE=HDF_TO_NetCDF&VERSION=1.02", sep="")        
    }
    outfile <- ifelse(outfile=="",paste(fname,".nc",sep=""), outfile)
    
    download.file(src, outfile, method="internal", mode="wb")
    traster <- raster(outfile, varname=var)
    if (month %in% c(1,3,5,7,8,10,12)){
        multiplier <- 24*31
    } else if (month==2){
        multiplier <- ifelse(isLeapYear(year),24*29,24*28)
    } else {
        multiplier <- 24*30
    }
    traster <- traster*multiplier
    return(traster)
}

downloadTRMM.daily <- function(wdate, outfile=""){
    if (!require(ncdf)) stop("Package ncdf not found.")
    wdate <- as.Date(wdate)
    prevday <- wdate-1
    doy <- doyFromDate(prevday)
    fname <- paste("3B42_daily.",format.Date(wdate, "%Y.%m.%d"),".6.nc", sep="") 
    src <- paste("http://disc3.nascom.nasa.gov/daac-bin/OTF/HTTP_services.cgi?FILENAME=%2Fftp%2Fdata%2Fs4pa%2FTRMM_L3%2FTRMM_3B42_daily%2F",yearFromDate(prevday),"%2F",serialn(doy, width=3),"%2F3B42_daily.",format.Date(wdate, "%Y.%m.%d"),".6.bin&LABEL=3B42_daily.",format.Date(wdate, "%Y.%m.%d"),".6.nc&SHORTNAME=TRMM_3B42_daily&SERVICE=HDF_TO_NetCDF&VERSION=1.02",sep="")
    outfile <- ifelse(outfile=="", fname, outfile)
    if (!file.exists(outfile)){
        download.file(src, outfile, method="internal", mode="wb")    
    }
    return(raster(outfile))
}

#yrs <- 1998:2010
#mos <- 1:12
#for (yr in yrs){
#    for (mo in mos){
#        prec <- downloadTRMM.monthly(month=mo,year=yr)        
#    }
#} 


outdir <- "D:/projects/Climate/Database/Source/TRMM"

years <- 1998:2011
failed <- vector()
msql <- odbcConnect("geoclimadmin")
sqlClear(msql, "trmm_15m")
sqlQuery(msql, "ALTER TABLE trmm_15m DISABLE KEYS")
base15m <- disaggregate(raster(),fact=4)
for (yr in years){
    if(yr==2011){
        dates <- seq.Date(from=as.Date(paste(yr,"-1-1", sep="")), to=as.Date(paste(yr,"-6-30", sep="")), by="day")
    } else {
        dates <- seq.Date(from=as.Date(paste(yr,"-1-1", sep="")), to=as.Date(paste(yr,"-12-31", sep="")), by="day")    
    }
    
    for (i in 1:length(dates)){
        dt <- dates[i]
        show.message("Processing TRMM data for ",as.character(dt), eol="\n")
        fname <- paste(outdir,paste("3B42_daily.",format.Date(as.Date(dt), "%Y.%m.%d"),".6.nc", sep=""),sep="/")
        if (file.exists(fname)) {
            show.message("Reading ",basename(fname), eol="\n")
            rain <- raster(fname)
        } else {
            rain <- try(downloadTRMM.daily(dt, outfile=fname))    
        }
        
        if (class(rain)=="try-error"){
            next
        } else {
            #rain100 <- aggregate(rain25, fact=4)
            trmm <- as.data.frame(rep("default", ncell(rain)),stringsAsFactors=FALSE)
            colnames(trmm) <- "id"
            xy <- xyFromCell(rain, 1:ncell(rain))
            xy[xy[,"x"]>180,"x"] <- xy[xy[,"x"]>180,"x"]-360 
            trmm$cell <- cellFromXY(base15m,xy)
            trmm$wdate <- as.character(dt)
            show.message("Retrieving values from ", basename(fname), eol="\n")
            trmm$prec <- round(values(rain),2)
            show.message("Uploading ", nrow(trmm), " records to trmm_15m.", eol="\n")
            if(class(try(sqlSave(msql,trmm,"trmm_15m",append=TRUE, rownames=FALSE)))=="try-error"){
              show.message("Uploaded ", basename(fname), eol="\n")
              failed <- c(failed,dt)
              next  
            }  
            show.message("Uploaded ", basename(fname), eol="\n")            
        }
    }
}
sqlQuery(msql, "ALTER TABLE trmm_15m ENABLE KEYS")
odbcCloseAll()



