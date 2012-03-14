# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  1 March 2012
# Version 0.0.1
# Licence GPL v3
SM.new    <- 0
SM.update <- 1
SM.append <- 2

.upload <- function(con, wthdframe, tablename, savemode=SM.append){
	
	proc <- try(sqlSave(con, wthdframe, tablename, rownames=FALSE, append=TRUE))
	success <- class(proc)!="try-error"
	if(!success) show.message(proc, appendLF=TRUE)
	#TODO: support transaction
	#if (transaction) sqlQuery(con, 'START TRANSACTION')
	
	#if (success & transaction) {
	#	sqlQuery(con, 'COMMIT')
	#} else if (transaction){
	#	sqlQuery(con, 'ROLLBACK')
	#}
	return(success)
}

upload.nasa <- function(dbasecon, nasa, setname='nasa_1d', ...){
	# TODO: support transaction
    success <- FALSE

	if (class(nasa)=="weather"){
		inasa <- cbind(0,as.numeric(nasa@stn), nasa@w)
		colnames(inasa) <- c('id','cell', colnames(nasa@w))
		success <- .upload(dbasecon, inasa, tablename=setname)
	} 
    return(success)    
}

upload.gsod <- function(connectionstring, year, setname="gsod_xd", stationtable="stations", dldir=getwd(), rm.download=FALSE){    
    force.directories(dldir, recursive=TRUE)
    
    con <- odbcConnect(connectionstring)
    stations <- sqlFetch(con, stationtable, stringsAsFactors=FALSE)

    tarfile <- paste(dldir, "/gsod_", year, ".tar", sep="")
    dlstart <- Sys.time()
    if(!file.exists(tarfile)) {
        show.message("Downloading ", tarfile,eol="\n")
        withRetry(download.file(paste(GSOD.ftp, year, "/gsod_", year, ".tar", sep=""), destfile=tarfile, mode="wb"))        
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
        
        #Parsing the GSOD file
        for (i in 1:14){
            assign(GSOD.varrefs$variable[i], trim(substr(dlines[-1], GSOD.varrefs$stpos[i], GSOD.varrefs$enpos[i])))
            if(!is.na(GSOD.varrefs$missing[i])) {
                tmp <- get(GSOD.varrefs$variable[i])
                tmp[tmp==as.character(GSOD.varrefs$missing[i])] <- NA
                assign(GSOD.varrefs$variable[i],tmp)
            }
        }

        wdate <- as.Date(YEARMODA,"%Y%m%d")                            
        gsod <- as.data.frame(wdate)
        
        # CLEAN UP CLIMATE DATA
        gsod$tavg  <- round(FtoC(as.numeric(TEMP)),1)*10 # MEAN TEMP
        gsod$slpressure   <- as.numeric(SLP)*10  # SEA LEVEL PRESSURE
        gsod$stpressure   <- as.numeric(STP)*10  # STATION PRESSURE
        gsod$tdew  <- round(FtoC(as.numeric(DEWP)),1)*10  # MEAN DEW POINT
        gsod$visibility <- round((as.numeric(VISIB) * 1.609344),1)*10 # VISIBILITY
        
        ##############################################
        # WINDSPEED NEEDED IN ORYZA2k
        gsod$wind  <- round(as.numeric(WDSP) * 0.514444444,1)*10 # WIND SPEED
        gsod$maxwind <- round(as.numeric(MXSPD) * 0.514444444,1)*10  # MAX SUSTAINED SPEED
        gsod$gust  <- round(as.numeric(GUST) * 0.514444444,1)*10  # MAX GUST
      
        ##############################################
        # MAX T NEEDED IN ORYZA2k
        gsod$tmax   <- round(FtoC(as.numeric(MAX)),1)*10  # MAX T
      
        ##############################################
        # MIN 2 NEEDED IN ORYZA2k
        gsod$tmin   <- round(FtoC(as.numeric(MIN)),1)*10  # MIN T
    
        ##############################################
        # RAINFALL NEEDED IN ORYZA2k
        gsod$prec   <- round(as.numeric(PRCP)*100/25.4,1)*10  # RAINFALL
        
        ##############################################
        # SNOW DEPTH
        gsod$snowdepth   <- round(as.numeric(SNDP)*100/25.4,1)*10  # convert to mm
        
        indicators <- matrix(as.numeric(unlist(strsplit(FRSHTT,""))),byrow=TRUE, ncol=6)
        colnames(indicators) <- c("ifog","irain","isnow","ihail","ithunder","itornado") 
        gsod <- cbind(station_id, gsod, indicators)
        show.message("Uploading parsed data from ", stations$station_code[stations$station_id==station_id],eol="\r")
        sqlSave(con,gsod, tablename=setname, rownames=FALSE, append=TRUE)
        stations$begin[stations$station_id==station_id] <- min(stations$begin[stations$station_id==station_id],min(wdate),na.rm=TRUE)
        stations$end[stations$station_id==station_id] <- max(stations$end[stations$station_id==station_id],max(wdate),na.rm=TRUE)
        show.message("Updating station information",eol="\r")
        sqlUpdate(con,stations[stations$station_id==station_id,],stationtable)
        show.message("Upload for station ", stations$station_code[stations$station_id==station_id], " done! (",as.character(min(wdate))," to ",as.character(max(wdate)),")",eol="\n")
    }
    procend <- Sys.time()    
    gsodlog <- c(paste("Download Start:", dlstart),paste("Download End:",dlend),paste("Download time:", round(difftime(dlend,dlstart, unit="mins"),2),"mins.\n"),
    paste("Process Start:", procstart),paste("Process End:",procend),paste("Process time:", round(difftime(procend,procstart, unit="mins"),2),"mins.\n"),
    paste("Files in archive:", length(gzfiles)), paste("Files with no station info:", length(failed)),failed)
    writeLines(gsodlog, paste(dldir,paste("gsod_log_",year,".txt",sep=""),sep="/"))
    if (rm.download) unlink(tarfile)                                                                                                
    unlink(gzdir,recursive=TRUE)
    # STATION UPDATE
    con <- odbcClose(con)
}   
