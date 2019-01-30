metaextract.cordex <- function(x=NULL, experiment="rcp45", verbose=FALSE) {
  if(verbose) print("metaextract.cordex")
  ## argument 'x' is input from getGCMs, getRCMs, testGCM, etc
  if (is.null(x)) x <- getRCMs(experiment=experiment, verbose=verbose)
  
  if(!inherits(x,"list")) x <- list(gcm.1=x)
  gcms <- names(x)
  n <- length(gcms)
  
  for (i in 1:n) {
    xx <- x[[gcms[i]]]
    project_id <- NA; url <- NA; filename <- NA; dim <- NA; dates <- NA
    var <- NA; longname <- NA; vunit <- NA; vid <- NA
    res <- NA; lon.rng <- NA; lon.unit <- NA; lat.rng <- NA; lat.unit <- NA
    experiment_id <- NA; frequency <- NA; creation_date <- NA; tracking_id <- NA
    gcm <- NA; gcm.rip <- NA; gcm.v <- NA; gcm.realm <- NA
    qf <- NULL;
    if(!is.null(xx$dim)) dim <- paste(names(xx$dim),collapse=",")[!grepl("bnds",names(xx$var))]
    if(!is.null(names(xx$var))) {
      var <- names(xx$var)[!grepl("bnds",names(xx$var))]
      if(!is.null(xx$var[[1]]$longname)) longname <- sapply(var, function(x) xx$var[[x]]$longname)
      if(!is.null(xx$var[[1]]$units)) vunit <- sapply(var, function(x) xx$var[[x]]$units)
      if(!is.null(xx$var[[1]]$id$id)) vid <- sapply(var, function(x) xx$var[[x]]$id$id)
    }
    if(!is.null(names(xx$dim))) {
      if(!is.null(xx$dim$lat$vals)) {
        res <- diff(xx$dim$lat$vals)[1]
        lat.rng <- paste(range(xx$dim$lat$vals),collapse=",")
      }
      if(!is.null(xx$dim$lon$vals)) lon.rng <- paste(range(xx$dim$lon$vals),collapse=",")
      if(!is.null(xx$dim$lat$units)) lat.unit <- xx$dim$lat$units
      if(!is.null(xx$dim$lon$units)) lon.unit <- xx$dim$lon$units
    }
    for(mi in c("url","filename","dates")) {
      if(!is.null(xx[[mi]])) eval(parse(text=paste(mi," <- xx$",mi,sep="")))
    }
    for(mi in c("project_id","experiment","experiment_id","frequency",
                "creation_date","tracking_id")) {
      if(!is.null(xx$model[[mi]])) eval(parse(text=paste(mi," <- xx$model$",mi,sep="")))
    }
    if(!is.null(xx$model$driving_model_id)) gcm <- xx$model$driving_model_id
    if(!is.null(xx$model$driving_model_ensemble_member)) gcm.rip <- xx$model$driving_model_ensemble_member
    if(!is.null(xx$model$model_id)) rcm <- xx$model$model_id
    if(!is.null(xx$model$CORDEX_domain)) rcm.domain <- xx$model$CORDEX_domain
    if(!is.null(xx$model$rcm_version_id)) rcm.v <- xx$model$rcm_version_id
    if(!is.na(filename)) filename <- gsub(".*/","",filename)
    # Check emission scenario against model history and correct when mislabeled:
    if(!grepl("rcp",experiment) & grepl("rcp",xx$model$history)) {
      h <- strsplit(xx$model$history," ")
      fname <- unlist(h)[grep("rcp.*.nc",unlist(h))[1]]
      experiment <- toupper(substr(fname,regexpr("rcp",fname)[1],
                                   regexpr("rcp",fname)[1]+4))
      N <- nchar(experiment)
      experiment <- paste(substr(experiment,1,N-1),substr(experiment,N,N),sep=".")
      experiment_id <- paste("historical",substr(fname,regexpr("rcp",fname)[1],
                                                 regexpr("rcp",fname)[1]+4),sep="+")
      qf <- c(qf,"Scenario (experiment, experiment_id) mislabeled in netCDF header.")
    }
    
    mx <- data.frame(project_id=project_id, url=url, filename=filename,
                     dim=paste(dim,collapse=","), dates=dates, var=paste(var,collapse=","),
                     longname=paste(longname,collapse=","), unit=paste(vunit,collapse=","), 
                     resolution=res, lon=lon.rng, lon_unit=lon.unit, lat=lat.rng, lat_unit=lat.unit,
                     experiment=experiment, experiment_id=experiment_id, frequency=frequency, 
                     creation_date=creation_date, 
                     gcm=gcm, gcm_rip=gcm.rip, rcm=rcm, qf=paste(qf,collapse="; "))
    meta <- names(mx)
    m <- length(meta)
    if (i==1) {
      X <- matrix(rep("NA",n*m),n,m) ## set up a matrix
      colnames(X) <- meta; rownames(X) <- gcms
    }
    for (ii in 1:m) {
      if(!is.na(mx[[meta[ii]]])) {
        y <- as.character(mx[[meta[ii]]])
        X[i,ii] <- y
      }
    }
  }
  return(X)
}
