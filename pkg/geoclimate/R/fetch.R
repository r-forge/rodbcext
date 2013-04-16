# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  14 March 2013
# Version 0.0.1
# Licence GPL v3

.fetch <- function(cells, con, wset, stdate=as.Date("2012-1-1"), endate=as.Date("2012-12-31"), vars=NULL, ...){
	#function(cells, con, wset, stdate=Sys.Date()-as.numeric(format(Sys.Date(),"%j"))+1, endate=Sys.Date(), vars=NULL, ...){
		
	# Preventive measures for known MySQL-born issues (i.e cannot open table, disconnected RODBC object) 
	preventivem <- try(sqlQuery(con, "flush tables"))	
	if (class(preventivem)=="try-error") {
		con <- odbcReConnect(con)
	}
	
	# remove invalid (NAs) cells
	cells <- cells[!is.na(cells)]
	
	# parameter vars general checks and query component construction 
	if (length(vars)>1){
		invalids <- which(is.na(vars))
		if (length(invalids)>0) stop ("Invalid vars specification detected.") else vars <- c("cell", "wdate", vars)		
	} else if (length(vars)==0){
		vars <- "*"
	} else if ((length(vars)==1 & (is.na(vars) | tolower(vars)=="all" | vars=="*"))){
		vars <- "*"
	} else {
		vars <- c("cell", "wdate", vars)
	}
	
	vars <- paste(vars, collapse=", ")
	query <- paste("SELECT", vars, "FROM", wset, "WHERE (wdate BETWEEN", shQuote(stdate), "AND", shQuote(endate),") AND (cell IN (",paste(cells, collapse=", ") ,")) ORDER BY cell, wdate")
	
	data <- sqlQuery(con, query, ...)
	
	return(data)
}

if ( !isGeneric("geoclimate.fetch") ) {
	setGeneric("geoclimate.fetch", function(xy, srcvars, connection, ...)
				standardGeneric("geoclimate.fetch"))
}


setMethod("geoclimate.fetch", signature(xy="matrix", srcvars="list", connection="RODBC"),
		function(xy, srcvars, connection, ...){
			
			# Connect to database
			#connection <- odbcConnect(connection)
			
			# Get dataset meta data for location matching
			srcmeta <- sqlQuery(connection,paste("SELECT * FROM geowarehouse.climate_data WHERE table_name in (", paste(shQuote(unique(names(srcvars))),collapse=", "),")"), stringsAsFactors=FALSE)
			maxres <- NA
			for (i in 1:length(srcvars)){
				srcm <- srcmeta[srcmeta$table_name==names(srcvars)[i],]				
				if (srcm$type=="grid"){
					srcraster <- raster(xmn=srcm$xmin, xmx=srcm$xmax, ymn=srcm$ymin, ymx=srcm$ymax, nrow=srcm$nrow, ncol=srcm$ncol)
					baseraster <- raster()
					res(baseraster) <- res(srcraster)

					# determine psudo-station number (basegrid + basegridcell)
					if (is.na(maxres)|maxres>res(srcraster)[1]){
						maxres <- res(srcraster)[1]						
					} 
					
					cells <- cellFromXY(srcraster,xy)
					
					stdcells <- cellFromXY(baseraster,xy)
					tmp <- .fetch(cells=stdcells, con=connection, wset=paste(srcm$schema_name,srcm$table_name, sep=".") , vars=srcvars[[i]], ...)
					#tmp <- fetch(cells=cells, con=connection, wset=paste(srcm$schema_name,srcm$table_name, sep=".") , vars=srcvars[[i]]) 
					tmp$idx <- match(tmp$cell, stdcells) 
					tmp <- tmp[,-1]
					tmp[,srcvars[[i]]] <- tmp[,srcvars[[i]]]/srcm$zval
					
				} else {
					warning("Non-grid type dataset not yet supported. Skipping.")
					# TODO support point type 
					next
				}
				if (!exists("outdat")) outdat <- tmp else outdat <- merge(outdat, tmp, by=c("idx","wdate"), all=TRUE)
			}
			
			
			basegrid <- raster() 
			res(basegrid) <- maxres			
			cells <- cellFromXY(basegrid,xy)			
			stn <- ifelse(length(gregexpr("0",unlist(strsplit(as.character(maxres),"\\."))[2])[[1]])>1,maxres*3600,maxres*60) 
			station <- paste(stn, sprintf(paste("%0",nchar(ncell(basegrid)),"d",sep=""),cells),sep="")

			# Construct source string (srcstr) for remarks on weather object
			srcstr <- vector()
			for (i in 1:length(srcvars)){
				srcstr <- c(srcstr, paste(names(srcvars)[i],": ", paste(srcvars[[i]], collapse=", "), sep=""))
			}
			srcstr <- paste(srcstr, collapse="; ")
			
			#Disaggregate into sets by point
			outlist <- list()
			for (i in 1:nrow(xy)){
				wth <- new ("weather")
				wth@stn <- station[i]
				wth@rmk <- srcstr
				wth@lon <- xy[i,1]
				wth@lat <- xy[i,2]
				wth@alt <- -99
				wth@w <- outdat[outdat$idx==i,-1]					 
				outlist[[i]] <- wth
			}
			return(outlist)				
		}
)

#setMethod("geoclimate.fetch", signature(cell="numeric"),
#		function(cell, ...){
#			return(.fetch(cell=cell,...))				
#		}
#)
#
#setMethod("geoclimate.fetch", signature(cell="matrix"),
#	function(cell, ...){
#	
#	}
#)
#
#setMethod("geoclimate.fetch", signature(cell="data.frame"),
#		function(cell, ...){
#			
#})
#
#setMethod("geoclimate.fetch", signature(cell="RasterLayer"),
#		function(cell, ...){
#			
#})
#
