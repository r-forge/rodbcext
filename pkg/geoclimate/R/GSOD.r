# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  20 January 2012
# Version 0.0.1
# Licence GPL v3

# Current ftp site
GSOD.ftp <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod/"

# Setup GSOD tables on Climate Schema
GSOD.setup <- function(connectionstring){
    # read station inventory
    show.message("Reading station information from GSOD.",eol="\n")
    stations <-  read.csv(paste(GSOD.ftp,"ish-history.csv",sep=""), stringsAsFactors=FALSE)
    show.message("Parsing information.",eol="\n")
    stations <- recodeMissing(stations,colnames(stations),"")
    stations <- recodeMissing(stations,colnames(stations),"??")
    
    station_id <- 1:nrow(stations)
    station_code <- paste(sprintf("%06d",stations$USAF),sprintf("%05d",stations$WBAN),sep="-")
    
    stations$LAT <- ifelse(stations$LAT > 90.0*1000|stations$LAT < -90.0*1000, NA, stations$LAT/1000)
    stations$LON <- ifelse(stations$LON > 180*1000|stations$LON < -180*1000, NA, stations$LON/1000)
    stations$ELEV..1M. <- ifelse(stations$ELEV..1M.==-99999|stations$ELEV..1M.==-999.999, NA, stations$ELEV..1M./10)
    #stations$BEGIN[!is.na(stations$BEGIN)] <- paste(substr(stations$BEGIN[!is.na(stations$BEGIN)],1,4),substr(stations$BEGIN[!is.na(stations$BEGIN)],5,6),substr(stations$BEGIN[!is.na(stations$BEGIN)],7,8),sep="-")
    #stations$BEGIN <- as.Date(stations$BEGIN)
    stations$BEGIN <- NA
    #stations$END[!is.na(stations$END)] <- paste(substr(stations$END[!is.na(stations$END)],1,4),substr(stations$END[!is.na(stations$END)],5,6),substr(stations$END[!is.na(stations$END)],7,8),sep="-")
    #stations$END <- as.Date(stations$END)
    stations$END  <- NA
    stations <- cbind(station_id, station_code, stations[,-which(colnames(stations) %in% c("USAF","WBAN"))],stringsAsFactors=FALSE)
    show.message("Connecting to geoclimate server.",eol="\n")
    con <- odbcConnect(connectionstring)
    show.message("Creating stations table.",eol="\n")
    sqlQuery(con, "DROP TABLE IF EXISTS `stations`")
    sqlQuery(con, paste(
        "CREATE TABLE `stations` (",
          "`station_id` int(11) NOT NULL,",
          "`station_code` char(12) NOT NULL COMMENT 'USAF-WBAN',",          
          "`stationname` varchar(50) DEFAULT NULL,",
          "`ctry` char(2) DEFAULT NULL,",
          "`fips` char(2) DEFAULT NULL,",
          "`state` char(2) DEFAULT NULL,",
          "`call` varchar(15) DEFAULT NULL,",
          "`lat` DECIMAL(6,3) DEFAULT NULL,",
          "`lon` DECIMAL(6,3) DEFAULT NULL,",
          "`elev1m` DECIMAL(10,3) DEFAULT NULL,",
          "`begin` DATE DEFAULT NULL,",
          "`end` DATE DEFAULT NULL,",
          "PRIMARY KEY (`station_id`)",
        ") ENGINE=MyISAM"))
    show.message("Sending station info to server.",eol="\n")
    sqlSave(con, stations, rownames=FALSE, append=TRUE)
    show.message("Creating gsod_xd datatable.",eol="\n")
    sqlQuery(con, "DROP TABLE IF EXISTS `gsod_xd`")
    sqlQuery(con, paste("CREATE TABLE `gsod_xd` (",
          "`station_id` INT(11) NOT NULL,",
          "`wdate` DATE NOT NULL,",
          "`tavg` INT DEFAULT NULL,",
          "`slpressure` INT DEFAULT NULL,",
          "`stpressure` INT DEFAULT NULL,",
          "`tdew` INT DEFAULT NULL,",
          "`visibility` INT DEFAULT NULL,",
          "`wind` INT DEFAULT NULL,",
          "`maxswind` INT DEFAULT NULL,",
          "`gust` INT DEFAULT NULL,",
          "`tmax` INT DEFAULT NULL,",
          "`tmin` INT DEFAULT NULL,",
          "`prec` INT DEFAULT NULL,",
          "`snowdepth` INT DEFAULT NULL,",
          "`ifog` BOOLEAN DEFAULT NULL,",
          "`irain` BOOLEAN DEFAULT NULL,",
          "`isnow` BOOLEAN DEFAULT NULL,",
          "`ihail` BOOLEAN DEFAULT NULL,",
          "`ithunder` BOOLEAN DEFAULT NULL,",
          "`itornado` BOOLEAN DEFAULT NULL",
        ") ENGINE=MyIsam DEFAULT CHARSET=latin1"))
    con <- odbcClose(con)
    show.message("Ready for GSOD scraping",eol="\n")
}

GSOD.upload <- function(connectionstring, year, dldir=getwd()){
    force.directories(dldir, recursive=TRUE)
    
    con <- odbcConnect(connectionstring)
    stations <- sqlFetch(con, "stations", stringsAsFactors=FALSE)

    tarfile <- paste(dldir, "/gsod_", year, ".tar", sep="")
    dlstart <- Sys.time()
    if(!file.exists(tarfile)) {
        show.message("Downloading ", tarfile,eol="\n")
        withRetry(download.file(paste(GSOD.ftp, year, "/gsod_", year, ".tar", sep=""), destfile=tarfile, mode='wb'))
    }
    dlend <- Sys.time()
    gzdir <- paste(dldir, year, sep="/")
    force.directories(gzdir, recursive=TRUE)
    show.message("Decompressing gsod tar file",eol="\n")
    untar(tarfile,verbose=FALSE, exdir=gzdir, extras="--no-same-owner")
    gzfiles <- list.files(gzdir,pattern="*.*.gz")
    #gfile <- gzfiles[1]
    failed <- vector()
    procstart <- Sys.time()
    for (gfile  in gzfiles){
        
        show.message("Reading ", gfile,eol="\r")
        
        station_id <- stations$station_id[match(substr(gfile,1,12), stations$station_code)]
        if(is.na(station_id)) {
            failed <- c(failed, gfile)
            next    
        }
        
        gz <- gzfile(paste(gzdir,gfile,sep="/"))
        dlines <- readLines(gz)
        close(gz)
        dlines <- gsub("\\*", "", dlines)
        dlines[-1] <- gsub("[[:alpha:]]", "", dlines[-1])
        dhdr <- unlist(strsplit(dlines[1], split="[[:space:]]+"))
        ddata <- matrix(unlist(strsplit(dlines[-1], split="[[:space:]]+")),byrow=TRUE,ncol=22)
        ddata <- ddata[,-c(5,7,9,11,13,15)]
        if(is.null(nrow(ddata))) ddata <- t(ddata)
        colnames(ddata) <- dhdr
        
        wdate <- as.Date(ddata[,"YEARMODA"],"%Y%m%d")                            
        
        gsod_xd <- as.data.frame(wdate)
        
        # CLEAN UP CLIMATE DATA
        gsod_xd$tavg  <- ifelse(ddata[, "TEMP"]=="9999.9", NA, round((as.numeric(ddata[, "TEMP"])-32)*5/9,1)*10)  # MEAN TEMP
        gsod_xd$slpressure   <- ifelse(ddata[,"SLP"]=="9999.9",  NA, as.numeric(ddata[, "SLP"])*10)  # SEA LEVEL PRESSURE
        gsod_xd$stpressure   <- ifelse(ddata[,"STP"]=="9999.9",  NA, as.numeric(ddata[, "STP"])*10)  # STATION PRESSURE
        gsod_xd$tdew  <- ifelse(ddata[,"DEWP"]=="9999.9", NA, round((as.numeric(ddata[, "DEWP"])-32)*5/9,1)*10)  # MEAN DEW POINT
        gsod_xd$visibility <- ifelse(ddata[, "VISIB"]==999.9, NA, round((as.numeric(ddata[, "VISIB"]) * 1.609344),1)*10) # VISIBILITY
        
        ##############################################
        # WINDSPEED NEEDED IN ORYZA2k
        gsod_xd$wind  <- ifelse(ddata[, "WDSP"]=="999.9",  NA, round(as.numeric(ddata[, "WDSP"]) * 0.514444444,1)*10)  # WIND SPEED
        gsod_xd$maxswind <- ifelse(ddata[, "MXSPD"]=="999.9", NA, round(as.numeric(ddata[, "MXSPD"]) * 0.514444444,1)*10)  # MAX SUSTAINED SPEED
        gsod_xd$gust  <- ifelse(ddata[, "GUST"]=="999.9",  NA, round(as.numeric(ddata[, "GUST"]) * 0.514444444,1)*10)  # MAX GUST
      
        ##############################################
        # MAX T NEEDED IN ORYZA2k
        gsod_xd$tmax   <- ifelse(ddata[, "MAX"]=="9999",  NA, round((as.numeric(ddata[, "MAX"])-32)*5/9,1)*10)  # MAX T
      
        ##############################################
        # MIN 2 NEEDED IN ORYZA2k
        gsod_xd$tmin   <- ifelse(ddata[, "MIN"]=="9999",  NA, round((as.numeric(ddata[, "MIN"])-32)*5/9,1)*10)  # MIN T
    
        ##############################################
        # RAINFALL NEEDED IN ORYZA2k
        gsod_xd$prec   <- ifelse(ddata[, "PRCP"]=="99.9", NA, round(as.numeric(ddata[, "PRCP"])*100/25.4,1)*10)  # RAINFALL
        
        ##############################################
        # SNOW DEPTH
        gsod_xd$snowdepth   <- ifelse(ddata[, "SNDP"]=="999.9", NA, round(as.numeric(ddata[, "SNDP"])*100/25.4,1)*10)  # convert to mm
        
        indicators <- matrix(as.numeric(unlist(strsplit(ddata[, "FRSHTT"],""))),byrow=TRUE, ncol=6)
        colnames(indicators) <- c("ifog","irain","isnow","ihail","ithunder","itornado") 
        gsod_xd <- cbind(station_id, gsod_xd,indicators)
        show.message("Uploading parsed data from ", stations$station_code[stations$station_id==station_id],eol="\r")
        sqlSave(con,gsod_xd,rownames=FALSE, append=TRUE)
        stations$begin[stations$station_id==station_id] <- min(stations$begin[stations$station_id==station_id],min(wdate),na.rm=TRUE)
        stations$end[stations$station_id==station_id] <- max(stations$end[stations$station_id==station_id],max(wdate),na.rm=TRUE)
        show.message("Updating station information",eol="\r")
        sqlUpdate(con,stations[stations$station_id==station_id,],"stations")
        show.message("Upload for station ", stations$station_code[stations$station_id==station_id], " done! (",as.character(min(wdate))," to ",as.character(max(wdate)),")",eol="\n")
    }
    procend <- Sys.time()    
    gsodlog <- c(paste("Download Start:", dlstart),paste("Download End:",dlend),paste("Download time:", round(difftime(dlend,dlstart, unit="mins"),2),"mins.\n"),
    paste("Process Start:", procstart),paste("Process End:",procend),paste("Process time:", round(difftime(procend,procstart, unit="mins"),2),"mins.\n"),
    paste("Files in archive:", length(gzfiles)), paste("Files with no station info:", length(failed)),failed)
    writeLines(gsodlog, paste(dldir,paste("gsod_log_",year,".txt",sep=""),sep="/"))
    unlink(tarfile)                                                                                                
    unlink(gzdir,recursive=TRUE)
    # STATION UPDATE
    con <- odbcClose(con)
}   
