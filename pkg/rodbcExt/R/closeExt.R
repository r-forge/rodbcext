# Author: 	Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date: 24 February 2009
# Version 0.1.1  
# License GPL3

disconnect <- function(channel, clean=T, env=.GlobalEnv){
    odbcClose(channel)
    if (clean){
        objs <- ls(envir=env)
        for (obj in objs){            
            cobj <- get(obj, envir=env)
            if(class(cobj)=="RODBC"){
                if (cobj==channel){
                    rm(list=obj, envir=env)
                }
            }
        }   
    }    
}
