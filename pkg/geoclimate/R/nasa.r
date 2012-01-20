# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

getNASA <- function(cell, stdate="1983-1-1", enddate=Sys.Date(), savefile=TRUE, savedir=".", redownload=FALSE){
    fname <- paste("nasa_",cell,".txt", sep="")
    
    stdate <- as.Date(stdate)
    enddate <- as.Date(enddate)

    #if (fname %in% list.files(savedir,pattern="nasa.*.txt") & !reupload){
    #    show.message(paste(cell, "done"), eol="\n")
    #    success <- TRUE
    #    return(success)
    #} else if(fname %in% list.files(savedir,pattern="nasa.*.txt") & reupload){
    #    show.message(paste("Reading ", fname, sep=""), eol="\n")
    #                   
    #} else {        
    #    if (verbose) show.message(paste("Downloading: Cell# ", cell," (",xy[1,"y"],",",xy[1,"x"], ")", sep=""), eol="\n")
    #    dlines <- readURL(paste("http://earth-www.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat=",xy[1,"y"],"&lon=",xy[1,"x"],"&ms=",monthFromDate(stdate),"&ds=",dayFromDate(stdate),"&ys=",yearFromDate(stdate),"&me=",monthFromDate(enddate),"&de=",dayFromDate(enddate),"&ye=",yearFromDate(enddate),"&p=swv_dwn&p=T2M&p=T2MN&p=T2MX&p=RH2M&p=DFP2M&p=RAIN&p=WS10M&submit=Submit", sep=""), verbose=TRUE)
    #}

    if(file.exists(paste(savedir,fname,sep="/")) & !redownload) {
        dlines <- readLines(paste(savedir,fname,sep="/"))
    } else {
        xy <- xyFromCell(raster(),cell)
        dlines <- readURL(paste("http://earth-www.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%40larc.nasa.gov&step=1&lat=",xy[1,"y"],"&lon=",xy[1,"x"],"&ms=",monthFromDate(stdate),"&ds=",dayFromDate(stdate),"&ys=",yearFromDate(stdate),"&me=",monthFromDate(enddate),"&de=",dayFromDate(enddate),"&ye=",yearFromDate(enddate),"&p=swv_dwn&p=T2M&p=T2MN&p=T2MX&p=RH2M&p=DFP2M&p=RAIN&p=WS10M&submit=Submit", sep=""), verbose=TRUE)
    }  
    
    if (savefile){
        force.directories(savedir, recursive=TRUE)
        writeLines(dlines, paste(savedir,"/nasa_",cell,".txt",sep=""))  
    }

    endline <- grep(paste(yearFromDate(enddate),format(doyFromDate(enddate),width=3)), dlines)
    if(length(dlines)==0 | length(endline)==0){
        if (verbose) warning("Empty or Incomplete data.")
        if (file.exists(paste(savedir,"/nasa_",cell,".txt",sep=""))) file.remove(paste(savedir,"/nasa_",cell,".txt",sep=""))        
    } else {
        ehdr <- grep("-END HEADER-", dlines)
        #hdr <- grep("YEAR DOY swv_dwn     T2M    T2MN    T2MX    RH2M   DFP2M    RAIN   WS10M", dlines)    

        if (length(ehdr)==0){
            if (verbose) show.message("Unrecognized format.", eol="\n")
            if (file.exists(paste(savedir,"/nasa_",cell,".txt",sep=""))) file.remove(paste(savedir,"/nasa_",cell,".txt",sep=""))
        }

        dlines <- dlines[(ehdr+1):endline]
        dvector <- unlist(strsplit(gsub("[[:space:]]+"," ",dlines), " "))
        dvector[dvector=="-"] <- NA
        nasadata <- as.data.frame(matrix(as.numeric(dvector), ncol=10, byrow=TRUE))
        colnames(nasadata) <- c("yr", "doy", "srad", "tavg", "tmin", "tmax", "rh2m", "tdew", "prec", "wind")
        nasadata <- cleanDframe(nasadata, cols=colnames(nasadata)[-(1:2)], rmOtherCols=FALSE)
        wdate <- as.character(dateFromDoy(nasadata$doy, nasadata$yr))
        nasadata <- cbind(cell, wdate, nasadata[,-(1:2)], stringsAsFactors=FALSE)
        rm(dlines,dvector)
        gc(verbose=FALSE)
    }           
    return(nasadata) 
}

uploadNASA <- function(con, setname, update=TRUE, verbose=TRUE, ...){
    
    success <- FALSE
    #forupload <- cbind(0,getNASA(...))
    if (!update){
        sqlQuery(con, paste("DELETE FROM",setname, "WHERE cell =",cell))
    } 
    id <- 0                
    forupload <- cbind(id,getNASA(...))
    if (verbose) show.message(paste("Uploading Records #", nrow(forupload), sep=""), eol="\n")
    try1 <- 1
        repeat {
            uploaded <- try(sqlSave(con, forupload, setname,rownames=FALSE, append=TRUE, fast=TRUE), silent=!verbose)
            if (class(uploaded)!="try-error"){
                success <- TRUE
                break
            } else if (try1 < 2){
                con <- odbcReConnect(con)
                try1 <- try1+1                
            } else {
                break
            }
        }
    #Check completeness of data
    return(success)    
}

