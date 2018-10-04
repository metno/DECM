## helpers.R
## Help functions for the shiny app "gcmeval"
library(plotrix)

Seasons <- list("ann",c('dec','jan','feb'),c('mar','apr','may'),c('jun','jul','aug'),c('sep','oct','nov'))
Periods <- list("nf","ff")

## Load statistics calculated with script 'calculate_statistics.R'
stats <- NULL
data("statistics.cmip.era.tas.1981-2010")
stats$tas$present <- store
data("statistics.cmip.tas.2021-2050")
stats$tas$nf <- store
data("statistics.cmip.tas.2071-2100")
stats$tas$ff <- store
data("statistics.cmip.era.pr.1981-2010")
stats$pr$present <- store
data("statistics.cmip.pr.2021-2050")
stats$pr$nf <- store
data("statistics.cmip.pr.2071-2100")
stats$pr$ff <- store

#reference data
tasref <- names(stats$tas$present)[!grepl("gcm",names(stats$tas$present))]
prref <- names(stats$pr$present)[!grepl("gcm",names(stats$pr$present))]

## Load metadata & subset common (pr and tas) GCMs
data("metaextract")
im <- meta$project_id=="CMIP5" & meta$var=="tas" 
im2 <- meta$project_id=="CMIP5" & meta$var=="pr"
synch <- match(paste(meta$gcm[im2],meta$gcm_rip[im2]),paste(meta$gcm[im],meta$gcm_rip[im]))
gcmnames <- paste(seq(length(synch)),": ",meta$gcm[synch],".",meta$gcm_rip[synch],sep="")

#remove not common GCMs from stats
reject <- which(!seq(length(meta$gcm[im])) %in% synch)
count=0
for (i in reject) {
  stats$tas$present[[i-count]] <- NULL
  stats$tas$nf[[i-count]] <- NULL
  stats$tas$ff[[i-count]] <- NULL
  count = count+1
}
######

#gcm names
gcmst <- names(stats$tas$ff)
gcmsp <- names(stats$pr$ff)

if (length(gcmst) != length(gcmsp)) {
  print("Number of GCMs with pr not equal to numbers of GCMS with tas!")
  #exit
}

## Load geographical data for map
data("geoborders",envir=environment())

regions <- function(type=c("srex","prudence"),region=NULL) {
  if(is.null(type) | length(type)>1) region <- NULL
  if(is.null(type) | "srex" %in% tolower(type)) {
    f <- "referenceRegions.shp"#find.file("referenceRegions.shp")
    x <- get.shapefile(f,with.path=TRUE)
    ivec <- 1:nrow(x)
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% x$LAB) {
        ivec <- sapply(region, function(y) which(y==x$LAB))
      } else if(region %in% x$NAME) {
        ivec <- sapply(region, function(y) which(y==x$NAME))
      } else {
        print(paste("Unknown region",region))
      }
    }
    y <- list(name=as.character(x$NAME[ivec]), 
              label=as.character(x$LAB[ivec]), 
              usage=as.character(x$USAGE[ivec]),
              type=rep("srex",length(ivec)),
              coords=lapply(ivec, function(i) t(coordinates(x@polygons[[i]]@Polygons[[1]]))))
  } else {
    y <- NULL
  }
  if(is.null(type) | "prudence" %in% tolower(type)) {
    f <- "RegionSpecifications.csv"#find.file("RegionSpecifications.csv")
    x <- read.table(f,sep=",")
    ivec <- 2:nrow(x)
    names <- as.character(x[2:nrow(x),1])
    labels <- as.character(x[2:nrow(x),2])
    if(!is.null(region)) {
      if(is.numeric(region)) {
        ivec <- region
      } else if(region %in% labels) {
        ivec <- sapply(region, function(y) which(y==labels)+1)
      } else if(region %in% names) {
        ivec <- sapply(region, function(y) which(y==names)+1)
      } else {
        print(paste("Unknown region",region))
      }
    }
    prudence <- list(name=as.character(x[ivec,1]),
                  label=as.character(x[ivec,2]),
                  usage=rep("land",length(ivec)),
                  type=rep("prudence",length(ivec)),
                  coords=lapply(ivec, function(i) 
                    t(matrix(sapply(c(4,5,5,4,4,6,6,7,7,6), 
                             function(j) factor2numeric(x[i,j])),
                             nrow=5,ncol=2))))
                  #west=as.numeric(x[2:nrow(x),4]),
                  #east=as.numeric(x[2:nrow(x),5]),
                  #south=as.numeric(x[2:nrow(x),6]),
                  #north=as.numeric(x[2:nrow(x),7]))
    if(is.null(y)) {
      y <- prudence 
    } else {
      y <- mapply(c, y, prudence, SIMPLIFY=FALSE)
    }
  }
  invisible(y)
}

## Function 'regions' is defined in global.R
srex <- regions("srex")

#model ranks for seasons, metrics and selected focus regions
ranking <- function(measure="bias",varid="tas",season="ann",region="global",im=NULL) {
  X <- switch(varid, tas=stats$tas$present, pr=stats$pr$present)
  if(tolower(region=="global")) {
    region <- "global"
  } else {
    i.srex <- which(srex$name==region)
    region <- srex$label[i.srex]
  }
  if (!season %in% names(X$gcm.1$global$mean)) {
    season <- switch(season, "djf" = c("dec","jan","feb"), "mam"=c("mar","apr","may"),
                     "jja" = c("jun","jul","aug"), "son"=c("sep","oct","nov"))
  }
  gcms <- names(X)[grepl("gcm",names(X))]
  ref <- names(X)[!grepl("gcm",names(X))]
  if(is.null(im)) im <- 1:length(gcms)
  if(ref=="era.pr") {
    X.ref <- X[ref]
    X.ref <- lapply(X[ref],function(x) lapply(x, function(y) lapply(y, function(z) z*1E3/(60*60*24))))
    X[ref] <- X.ref
  }
  if(measure=="bias") {
    skill <- rank(abs(sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
      X[[gcm]][[region]][["mean"]][[s]]-X[[ref]][[region]][["mean"]][[s]])))))
  } else if (measure=="sd.ratio") {
    skill <- rank(abs(1-sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
      X[[gcm]][[region]][["spatial.sd"]][[s]]/X[[ref]][[region]][["spatial.sd"]][[s]])))))
  } else if (measure=="corr") {
    skill <- rank(1-sapply(gcms[im], function(gcm) mean(sapply(season, function(s)
      X[[gcm]][[region]][[measure]][[s]]))))
  } else if (tolower(measure) %in% c("cmpi","e")) {
    skill <- rank(-1*sapply(gcms[im], function(gcm) X[[gcm]][[region]][[measure]]))
  }
  return(skill)
}

ranking.all <- function(varid="tas",Regions=list("global","Amazon [AMZ:7]"),Seasons=c("ann","djf","mam","jja","son"),im=NULL) {
  gcms <- names(stats[[varid]]$present)
  gcms <- gcms[grepl("gcm",gcms)]
  if(!is.null(im)) im <- 1:length(gcms)
  Ranks <- array(NA,c(length(gcms),length(Seasons),4,length(Regions)))
  for (si in 1:length(Seasons)) {
    for (ri in 1:length(Regions)) {
      Ranks[,si,1,ri] <- ranking(measure="bias",varid=varid,season=Seasons[si],region=Regions[ri],im=im)
      Ranks[,si,2,ri] <- ranking(measure="sd.ratio",varid=varid,season=Seasons[si],region=Regions[ri],im=im)
      Ranks[,si,3,ri] <- ranking(measure="corr",varid=varid,season=Seasons[si],region=Regions[ri],im=im)
      Ranks[,si,4,ri] <- ranking(measure="e",varid=varid,season=Seasons[si],region=Regions[ri],im=im)
    }
  }
  return(Ranks)
}

ranking.weighted <- function(regionwm1="global",regionwm2="Amazon [AMZ:7]",wmreg1=1,wmreg2=1,
                          wmdt=1,wmdp=1,wmann=1,wmdjf=1,wmmam=1,wmjja=1,wmson=1,
                          wmbias=1,wmsd=1,wmsc=1,wmcmpi=1,n.gcm=11) {
  
  Regionlist <- list(regionwm1,regionwm2)
  Regionlist <- Regionlist[which(Regionlist != "---")]
  
  tasRanks <- ranking.all(varid="tas",Regions=Regionlist,gcms=gcmst)
  prRanks <- ranking.all(varid="pr",Regions=Regionlist,gcms=gcmsp)

  #seasonal ranks, weighted for temp and precip
  seas_varweightedranks <- as.numeric(wmdt)*tasRanks+as.numeric(wmdp)*prRanks

  #seasonal weights
  seasweightvec <- as.numeric(c(wmann,wmdjf,wmmam,wmjja,wmson))

  #region weights
  regweightvecin <- as.numeric(c(wmreg1,wmreg2))
  regweightvec <- regweightvecin[which(Regionlist != "---")]

  #calculate weighted ranks for selection
  weightedranks_all <- array(NA,c(length(gcmst),4))

  for (i in 1:length(gcmst)) {
    for (j in 1:4) {
      weightedranks_all[i,j] <- (seasweightvec %*% seas_varweightedranks[i,,j,] %*% regweightvec)
    }
  }
  
  #weight metrics
  metweightvec <- as.numeric(c(wmbias,wmsd,wmsc,wmcmpi))
  weightedrank_all <- rank(weightedranks_all %*% metweightvec)
  
  best <- order(weightedrank_all)[1:n.gcm]
  
  return(weightedrank_all)
}

spread <- function(varid="tas",season="ann",region="global",period="ff",gcms=NULL) {
  X <- switch(varid, tas=stats$tas, pr=stats$pr)
  if(tolower(region=="global")) {
    region <- "global"
  } else {
    i.srex <- which(srex$name==region)
    region <- srex$label[i.srex]
  }
  if (!season %in% names(X[[period]]$gcm.1$global$mean)) {
    season <- switch(season,"ann"="ann", "djf" = c("dec","jan","feb"), "mam"=c("mar","apr","may"),
                     "jja" = c("jun","jul","aug"), "son"=c("sep","oct","nov"))
  }
  if(is.null(gcms)) gcms <- names(X)[grepl("gcm",names(X))]
  dX <- diff(range(sapply(gcms, function(gcm) mean(sapply(season, function(s)
            X[[period]][[gcm]][[region]][["mean"]][[s]])) - 
              mean(sapply(season, function(s) 
                X$present[[gcm]][[region]][["mean"]][[s]]))),na.rm=T))
  return(dX)
}

spread.all <- function(varid="tas",Regions=c("global","Amazon [AMZ:7]"),Seasons=list("ann","djf","mam","jja","son"),gcms=NULL) {
  Periods <- c("nf","ff")
  dX <- array(NA,c(length(Seasons),length(Periods),length(Regions)))
  dX.gcms <- array(NA,c(length(Seasons),length(Periods),length(Regions)))
  for (si in 1:length(Seasons)) {
    for (peri in 1:length(Periods)) {
      for (ri in 1:length(Regions)) {
        dX[si,peri,ri] <- spread(varid=varid,season=Seasons[[si]],region=Regions[ri],period=Periods[peri],gcms=gcms)
      }
    }
  }
  return(dX)
}

spread.weighted <- function(regionwm1="global",regionwm2="Amazon [AMZ:7]",wmreg1=1,wmreg2=1,
                            wmdt=1,wmdp=1,wmann=1,wmdjf=1,wmmam=1,wmjja=1,wmson=1,
                            wmbias=1,wmsd=1,wmsc=1,wmcmpi=1,gcms=NULL,n.gcm=11) {
  
  Regionlist <- list(regionwm1,regionwm2)
  Regionlist <- Regionlist[which(Regionlist != "---")]
  
  if(is.null(gcms)) gcms <- gcmnames[sample(1:length(gcmnames),11,replace=FALSE)]
  
  regweightvec <- as.numeric(c(wmreg1,input$wmreg2))
  regweightvec[which(list(regionwm1,regionwm2) != "---")] 
  
  dtasSpread <- spread.all(varid="tas",Regions=Regionlist,gcms=NULL)
  dtasSelSpread <- spread.all(varid="tas",Regions=Regionlist,gcms=gcms)
  dprSpread <- spread.all(varid="pr",Regions=Regionlist,gcms=NULL)
  dprSelSpread <- spread.all(varid="pr",Regions=Regionlist,gcms=gcms)
  
  dtasRelSpread <- dtasSelSpread/dtasSpread
  dprRelSpread <- dprSelSpread/dprSpread
  seas_varweightedspread <- (as.numeric(wmdt)*dtasRelSpread + 
                             as.numeric(wmdp)*dprRelSpread)/
                            (as.numeric(wmdt)+as.numeric(wmdp))
  
  weightedspread_nf <- (seasweightvec %*% seas_varweightedspread[,1,] %*% regweightvec)/
      sum(seasweightvec)/sum(regweightvec)
  weightedspread_ff <- (seasweightvec %*% seas_varweightedspread[,2,] %*% regweightvec)/
      sum(seasweightvec)/sum(regweightvec)
  
  return(list(ff=weightedspread_ff, nf=weightedspread_nf))
}



#Regionlist <- reactive({list(input$regionwm1,input$regionwm2)})
#Regions <- reactive({Regionlist[which(Regionlist != "---")]})#Only those different from "---"

#total ensemble and model selection spread for seasons, periods and selected focus regions
#dtasSpread <- array(NA,c(5,2,length(Regions)))
#dprSpread <- array(NA,c(5,2,length(Regions)))
#dtasSelSpread <- array(NA,c(5,2,length(Regions)))
#dprSelSpread <- array(NA,c(5,2,length(Regions)))

#Seasons <- list("ann",c('dec','jan','feb'),c('mar','apr','may'),c('jun','jul','aug'),c('sep','oct','nov'))
#Periods <- list("nf","ff")

#for (si in 1:5)
#{
#  for (peri in 1:2)
#  {
#    for (ri in 1:length(Regions))
#    {
#      if(tolower(Regions[[ri]])=="global") {
#        region <- "global"
#      } else {
#        i.srex <- which(srex$name==Regions[[ri]])
#        region <- srex$label[i.srex]
#      }

#      dtasSpread[si,peri,ri] <- diff(range(sapply(gcmst, function(gcm) mean(sapply(Seasons[[si]], function(s)
#        stats$tas[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
#          mean(sapply(Seasons[[si]], function(s) 
#            stats$tas$present[[gcm]][[region]][["mean"]][[s]]))),na.rm=T))
#      dprSpread[si,peri,ri] <- diff(range(sapply(gcmsp, function(gcm) mean(sapply(Seasons[[si]], function(s)
#        stats$pr[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
#          mean(sapply(Seasons[[si]], function(s) 
#            stats$pr$present[[gcm]][[region]][["mean"]][[s]]))),na.rm=T))*60*60*24


#      dtasSelSpread[si,peri,ri] <- diff(range(sapply(gcmst, function(gcm) mean(sapply(Seasons[[si]], function(s)
#        stats$tas[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
#          mean(sapply(Seasons[[si]], function(s) 
#            stats$tas$present[[gcm]][[region]][["mean"]][[s]])))[im],na.rm=T))
#      dprSelSpread[si,peri,ri] <- diff(range(sapply(gcmsp, function(gcm) mean(sapply(Seasons[[si]], function(s)
#        stats$pr[[Periods[[peri]]]][[gcm]][[region]][["mean"]][[s]])) - 
#          mean(sapply(Seasons[[si]], function(s) 
#            stats$pr$present[[gcm]][[region]][["mean"]][[s]])))[im],na.rm=T))*60*60*24
#    }
#  }
#}

#seasonal, relative model selection spread
#dtasRelSpread <- dtasSelSpread/dtasSpread
#dprRelSpread <- dprSelSpread/dprSpread



