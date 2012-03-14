# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  21 February 2012
# Version 0.0.1
# Licence GPL v3

rescale <- function(x, oldmin, oldmax, newmin, newmax){
  return((x-oldmin)*(newmax-newmin)/(oldmax-oldmin) + newmin)
}
