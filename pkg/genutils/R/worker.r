# Author: Jorrel Khalil S. Aunario, jaunario@gmail.com
# Date :  27 March 2014
# Version 0.0.1
# Licence GPL v3

get.jobs <- function(initjobs, jobfile="jobs.Rdata",workload=500,delay=10, maxtries=100){
	myjob <- vector()
	worker.id <- Sys.getpid()
	
	if(!file.exists(jobfile)){
		jobs <- initjobs
		save(jobs, file=jobfile)
		
		filelock <- data.frame(filename=character(0),worker=numeric(0))
		filelock[1,] <- NA
		filelock$filename[1] <- jobfile   
		write.csv(filelock,"files.csv",row.names=FALSE)		
 
	}
	
	tries <- 0
	repeat{
		filelock <- read.csv("files.csv",stringsAsFactors=FALSE)
		if(is.na(filelock$worker[filelock$filename==jobfile])){
			filelock$worker[filelock$filename==jobfile] <- worker.id
			write.csv(filelock,"files.csv",row.names=FALSE)
			load(jobfile)
			myjob <- jobs[1:min(workload,length(jobs))] 
			jobs <- jobs[!jobs %in% myjob]
			save(jobs, file=jobfile)
			filelock$worker[filelock$filename==jobfile] <- NA
			write.csv(filelock,"files.csv",row.names=FALSE)
			break
		} else if (tries<maxtries){
			message("Job directory currently in use. Waiting.", appendLF=TRUE)
			Sys.sleep(delay)	
			tries <- tries+1
		} else {
			message("Too many workers queued. Try again next time.", appendLF=TRUE)
			break
		}	
	}
	return(myjob)
}
