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

upload.nasa <- function(dbasecon, nasa, setname='nasa_1d'){
	# TODO: support transaction
    success <- FALSE

	if (class(nasa)!="weather"){
		stop("Invalid nasa input. Should be class 'weather'")
	} 
	
	inasa <- cbind(0,as.numeric(nasa@stn), nasa@w)
	colnames(inasa) <- c('id','cell', colnames(nasa@w))
	success <- .upload(dbasecon, inasa, tablename=setname)
    return(success)    
}

upload.gsod <- function(dbasecon, gsod, setname="gsod_xd"){    
	# TODO: support transaction
    success <- FALSE

	if (class(gsod)!="weather"){
		stop("Invalid gsod input. Should be class 'weather'")
	} 
	
	igsod <- cbind(as.numeric(gsod@stn), gsod@w)
	colnames(igsod) <- c('station_id', colnames(gsod@w))
	success <- .upload(dbasecon, igsod, tablename=setname)
    
	return(success)    
} 

upload.trmm <- function(dbasecon, trmm, setname="trmm_15m"){
	# TODO: support transaction
    success <- FALSE

	if (class(trmm)!="weather"){
		stop("Invalid gsod input. Should be class 'weather'")
	} 
	
	#igsod <- cbind(as.numeric(gsod@stn), gsod@w)
	#colnames(igsod) <- c('station_id', colnames(gsod@w))
	success <- .upload(dbasecon, trmm@w, tablename=setname)
    
	return(success)    

}  

upload.FSE <- function(dbasecon, clim, setname, stations=NA, has.AIid=FALSE){
	add <- success <- FALSE
	
		
	if(class(clim)!="weather"){
		stop("Invalid clim input. Should be class 'weather'")
	}
	
	if(class(dbasecon)!="RODBC"){
		stop("Invalid dbasecon input. Should be class 'RODBC'")
	}
	
	if (!isOpen(dbasecon)) dbasecon <- odbcReConnect(dbasecon)
	
	if(!is.na(stations) & clim@stn!="Unknown"){
		station_info <- sqlQuery(dbasecon, paste("SELECT * FROM", stations, "WHERE lat =",clim@lat, "AND lon =",clim@lon), stringsAsFactors=FALSE)
		
		if (nrow(station_info)==0){
			add <- TRUE 
			station_info[1,] <- NA
			station_info$station_id <- sqlQuery(dbasecon, paste("SELECT IF(MAX(station_id) IS NULL,1,MAX(station_id)+1) station_id FROM", stations))$station_id
			station_info$station_name <- clim@stn
			station_info$lat <- clim@lat
			station_info$lon <- clim@lon
			station_info$elev <- ifelse(clim@alt==-9999,NA,clim@alt)
			station_info$pixel_1d <- cellFromXY(raster(),station_info[,c("lon", "lat")])
			station_info$remarks <- ifelse(clim@rmk=="", NA, paste(Sys.time(), ": ", clim@rmk, sep=""))
		} else if(clim@rmk!="" & length(grep(clim@rmk,station_info$remarks))==0){
			station_info <-  station_info[,c("station_id","begin","end","remarks","updated")]
			station_info$remarks <- paste(station_info$remarks, "\n", Sys.time(),": ",clim@rmk, sep="")
		} else {
			station_info <- station_info[,c("station_id","begin","end", "updated")]
		}
		station_info$begin <- min(as.Date(station_info$begin), clim@w$wdate, na.rm=TRUE)
		station_info$end <- max(as.Date(station_info$end), clim@w$wdate, na.rm=TRUE)
		station_info$updated <- NA
		station_id <- station_info$station_id 
		dat <- cbind(station_id, clim@w)
	} else if(clim@stn!="Unknown"){
		stop("Invalid station name. Should not be 'Unknown'")
	} 	
	if(has.AIid){
		id <- 0
		dat <- cbind(id, dat)
	}
	
	success <- .upload(dbasecon, dat, tablename=setname)
	
	if (success & !is.na(stations) & add) sqlSave(dbasecon, station_info, tablename=stations, append=TRUE, rownames=FALSE) else if (success & !is.na(stations)) sqlUpdate(dbasecon, station_info, tablename=stations)
	return(success)
}