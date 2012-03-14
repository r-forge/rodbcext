# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  20 January 2012
# Version 0.0.1
# Licence GPL v3

# Current ftp site

get.gsod <- function(year, station, savepath=getwd(), rm.existing=FALSE){
	result <- vector()
	
	GSOD.ftp <- "ftp://ftp.ncdc.noaa.gov/pub/data/gsod"
	GSOD.varrefs <- read.csv(system.file("gsod_ref.csv", package="geoclimate"), stringsAsFactors=FALSE)

	if(!force.directories(savepath, recursive=TRUE)){
		show.message("Error: Can't create download path.", appendLF=TRUE)
	} else if(!require(RCurl)){
		show.message("Error: RCurl package not found.", appendLF=TRUE)
	} else {
		fname <- paste(station,"-",year,".op.gz", sep="")
		ftpurl <- paste(GSOD.ftp, year, fname, sep="/")
		available <- withRetry(getURL(paste(GSOD.ftp,"/",year,"/",sep="")))
		if (!grepl(station, available)){
			show.message("Data not available on ", station, " for year ", year,".", appendLF=TRUE)
		} else {
			dl.success <- withRetry(download.file(ftpurl, destfile=paste(savepath,fname, sep="/"), mode="wb"))
			
			# Parse the gsod file if successfully downloaded
			if (dl.success==0){
				gz <- gzfile(paste(savepath,fname,sep="/"))
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
				gsod <- cbind(gsod, indicators, stringsAsFactors=FALSE)
				result <- new('weather')
				result@stn <- station
				result@lon <- x
				result@lat <- y
				result@alt <- alt
				result@w <- gsod
			}
		}
		
	}
	return(result)
}
