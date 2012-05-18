# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  7 May 2012
# Version 0.0.1
# Licence GPL v3
# Read and Write FSE weather files

read.FSE <- function(fsefile, datacols=c("station_id", "year", "doy", "srad", "tmin", "tmax", "vaporp","wind","prec")){

	fsewth <-  new("weather")
	is.sunshine <- FALSE
	if (length(which(datacols %in% c("year", "doy")))!=2) stop("Required columns year and doy (day of year) not found.")

	if(file.exists(fsefile) & file.info(fsefile)$size!=0){
		
		dlines <- readLines(fsefile)
		dlines <- gsub("\t", " ", dlines)
		# get headers
		ihdr <- grep("\\*", dlines)
		hdr <- gsub("\\*", " ", dlines[min(ihdr):max(ihdr)])
		hdr <- trim(gsub("\\?", " ", hdr))
		hdr <- hdr[hdr!=""]
		
		icol <- grep("1[[:space:]]+Station", hdr, ignore.case=TRUE)
		if (length(grep("--", hdr))>0){
			colinfo <- hdr[icol:(length(hdr)-1)]
		} else{
			colinfo <- hdr[icol:length(hdr)]
		}
		hdr <- hdr[1:(icol-1)]
		# get station name
		i <- grep("station", hdr, ignore.case=TRUE)
		if (length(i)==0) {
			i <- grep("location", hdr, ignore.case=TRUE)
		} 
		fsewth@stn <- ifelse(!is.na(i[1]), trim(gsub("\\*", "", unlist(strsplit(hdr[i],":"))[2])),"Unknown")
		
		# get source
		i <- grep("source", hdr, ignore.case=TRUE)
		fsewth@rmk <- ifelse(length(i)==1, trim(unlist(strsplit(hdr[i],":"))[2]),"")
		
		# get station name
		#i <- grep("source", hdr, ignore.case=TRUE)
		#fsewth@rmk <- ifelse(length(i)==1, trim(unlist(strsplit(dlines[i],":"))[2]),"")
		
		# get coordinates
		coords <- as.numeric(unlist(strsplit(trim(dlines[max(ihdr)+1]),"[[:space:]]+")))
		rm(dlines)
		gc(verbose=FALSE)
		
		fsewth@lon <- coords[1]
		fsewth@lat <- coords[2]
		fsewth@alt <- coords[3]
		
		#dmatrix <- matrix(as.numeric(unlist(strsplit(trim(dlines[(max(ihdr)+2):length(dlines)]), "[[:space:]]+"))), ncol=length(colinfo), byrow=TRUE)
		#dmatrix[dmatrix==-9999] <- NA
		#dmatrix <- as.data.frame(dmatrix)
		
		dmatrix <- read.table(fsefile, skip=max(ihdr)+1, na.strings="-9999", stringsAsFactors=FALSE)
		colnames(dmatrix) <- datacols
				
		# CHECK RADIATION UNITS THEN CONVERT TO MEGAJOULE/SQM/DAY IF NECESSARY
		
		# Check if sunshine hours/duration
		rad_var <- grep("sunshine[[:print:]]*", tolower(colinfo), ignore.case=TRUE)		
		if (length(rad_var)!=0){
			dmatrix[,rad_var] <- round(sunhoursToSRad(dmatrix[,rad_var],dmatrix[,3],fsewth@lat, coords[4], coords[5]),2)
			show.message("Sunshine duration", appendLF=TRUE)
			
		} else {
			rad_var <- grep("[[:print:]]*rad[[:print:]]*", tolower(colinfo), ignore.case=TRUE)
			if(length(rad_var)!=0 & grepl("kj", colinfo[rad_var],ignore.case=TRUE)) {
				dmatrix[,rad_var] <- round(dmatrix[,rad_var]/1000,2)
			} 

		}
		
		wdate <- dateFromDoy(dmatrix[,"doy"],dmatrix[,"year"])
		fsewth@w <- cbind(wdate,as.data.frame(dmatrix[,4:length(datacols)]))		
		#fsewth@rmk <- ifelse(length(i)==1, trim(unlist(strsplit(dlines[i],":"))[2]),"")
		
	} else {
		stop(fsefile, " not found.")
	}
	return(fsewth)
}
