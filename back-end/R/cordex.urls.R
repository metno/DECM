cordex.urls <- function(experiment='rcp45',varid='tas',
                        url="http://climexp.knmi.nl/CORDEX/EUR-44/mon/",#path=NULL,
                        force=FALSE,verbose=FALSE) {
  if(verbose) print("cordex.urls")
  urlfiles <- c()
  #if(is.null(path)) path <- getwd()
  for (iexp in experiment) {
    if(verbose) print(iexp)
    for (ivar in varid) {
      if(verbose) print(ivar)
      ## Loop on the number of experiments
      for (irun in 0:20) { ## 
        if(verbose) print(paste(irun))
        ## Update experiment number
        if (irun < 10) run.id = paste("00",as.character(irun),sep="")
        else if (irun < 100) run.id = paste("0",as.character(irun),sep="")
        else run.id <- as.character(irun)
        
        urlfile  <- paste(url,ivar,sep="")             # add var directory
        urlfile  <- paste(urlfile,ivar,sep="/")        # add v.name
        urlfile  <- paste(urlfile,"EUR-44_cordex",sep="_") # add text
        urlfile  <- paste(urlfile,iexp,"mon",sep="_")         # add exp.name
        urlfile  <- paste(urlfile,run.id,sep="_")      # add exp ID number
        urlfile  <- paste(urlfile,".nc",sep="")        # add file ex
        if(RCurl::url.exists(urlfile)) {
          if (verbose) print(urlfile)
          urlfiles <- c(urlfiles,urlfile)
        }
      }
    }
  }
  return(urlfiles)
}