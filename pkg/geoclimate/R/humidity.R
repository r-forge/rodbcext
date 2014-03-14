# TODO: Add comment
# 
# Author: jaunario
###############################################################################



iRH <- function(temp,mvp){
	es_Ta <- svp(temp) #saturated water vapor pressure at Ta (hPa)
	
	# Instantaneous relative humidity, RHumi%)
	RHum <- mvp / es_Ta * 100
	if (RHum > 100) RHum <- 100
	return(RHum)
}
