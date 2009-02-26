# Author: 	Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date: 24 February 2009
# Version 0.1.1  
# License GPL3

cleanDisconnect <- function(channel, rmv=T, env=.GlobalEnv){
    odbcClose(channel)
    if (rmv){
        objs <- ls(envir=env)
        for (obj in objs){            
            cobj <- get(obj, envir=env)
            if(is.atomic(cobj)|is.list(cobj) ){
                if (cobj==channel){
                    rm(list=obj, envir=env)
                }
            }
        }   
    }    
}
