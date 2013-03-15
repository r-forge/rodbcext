# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  14 March 2013
# Version 0.0.1
# Licence GPL v3

.fetch <- function(cell, con, wset, stdate, endate, vars=NULL, ...){
	if (is.null(vars)|is.na(vars)) vars <- "*" else vars <- paste(vars, collapse=", ")
	query <- paste("SELECT", vars, "FROM", wset, "WHERE (wdate BETWEEN", stdate, "AND", endate,") AND (cell IN (",paste(cell, collapse=", ") ,")")
	data <- sqlQuery(con, query, ...)
	return(data)
}

setMethod("geoclimate.fetch", signature(x="integer"),
		function(x, ...){
			return (.fetch(cell=x,...))				
		}
)

setMethod("geoclimate.fetch", signature(x="matrix"),
	function(x, ...){
	
	}
)

setMethod("geoclimate.fetch", signature(x="data.frame"),
		function(x, ...){
			
})

setMethod("geoclimate.fetch", signature(x="RasterLayer"),
		function(x, ...){
			
})
