# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

get.nasa <- function(x, y, stdate="1983-1-1", endate=Sys.Date(), savepath=getwd(), rm.existing=FALSE){
	result <- vector()
	if(length(x)!=1|length(y)!=1){
		show.message("Warning: Either x or y has length > 1. Using first only.", appendLF=TRUE)
		x <- x[1]
		y <- y[1]
	}
	if(!force.directories(savepath)) {
		show.message("Error: Cannot create ", savepath, appendLF=TRUE)
	} else {
		stdate <- as.Date(stdate)
		endate <- as.Date(endate)
		
		fname <- paste(savepath, paste(paste("nasa",x,y,format(stdate,"%Y.%m.%d"),format(endate,"%Y.%m.%d"), sep="_"), ".txt",sep=""), sep="/")
		dlurl <- paste("http://earth-www.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat=",y,"&lon=",x,"&ms=",format(stdate,"%m"),"&ds=",format(stdate,"%d"),"&ys=",format(stdate,"%Y"),"&me=",format(endate,"%m"),"&de=",format(endate,"%d"),"&ye=",format(endate,"%Y"),"&p=swv_dwn&p=T2M&p=T2MN&p=T2MX&p=RH2M&p=DFP2M&p=RAIN&p=WS10M&submit=Submit", sep="")
		
		show.message("Reading ", appendLF=FALSE)
		if (!file.exists(fname)){
			show.message(dlurl, appendLF=TRUE)
			dlines <- withRetry(readLines(dlurl))
		} else if (rm.existing | file.info(fname)$size==0){
			file.remove(fname)
			show.message(dlurl, appendLF=TRUE)
			dlines <- withRetry(readLines(dlurl))
		} else {
			show.message(fname, appendLF=TRUE)
			dlines <- readLines(fname)
		}
		
		if (length(dlines)==0){
			stline <- endline <- vector()
		} else {					
			stline <- grep(paste(format(stdate,"%Y"),format(as.numeric(format(stdate,"%j")),width=3)), dlines)
			endline <- grep(paste(format(endate,"%Y"),format(as.numeric(format(endate,"%j")),width=3)), dlines)
		}
		
		if (length(stline)!=0 & length(endline)!=0) {
			writeLines(dlines, fname)
			alt <- as.numeric(unlist(strsplit(dlines[grep("Elevation", dlines)],"="))[2])
			dlines <- dlines[stline:endline]
			dvector <- unlist(strsplit(gsub("[[:space:]]+"," ",dlines), " "))
			dvector[dvector=="-"] <- NA
			nasadata <- as.data.frame(matrix(as.numeric(dvector), ncol=10, byrow=TRUE))
			colnames(nasadata) <- c("yr", "doy", "srad", "tavg", "tmin", "tmax", "rh2m", "tdew", "prec", "wind")
			wdate <- format(as.Date(paste(nasadata$yr,nasadata$doy),"%Y %j"),"%Y-%m-%d")
			nasadata <- cbind(wdate, nasadata[,-(1:2)], stringsAsFactors=FALSE)
			result <- new('weather')
			result@stn <- as.character(cellFromXY(raster(),t(c(x,y))))
			result@lon <- x
			result@lat <- y
			result@alt <- alt
			result@w <- nasadata
			rm(dlines,dvector,nasadata)
			gc(verbose=FALSE)
		}
	}
	return(result)
}
