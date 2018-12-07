## Function for generating urls to files available on a THREDDS data server (TDS). 
## Not sure if it works on other TDSs than the one here at metno.
## I had problems listing and downloading files from the same location on the metno TDS  
## and solved it by including two different base URLs, url.base and url.download. 
## Perhaps this can be done in an easier and more elegant way.
thredds.urls <- function(url.rel="raw/tas",pattern=".*EUR-11.*.nc",select=NULL,
                         url.base="http://thredds.met.no/thredds/catalog/postclim/data/CORDEX-EUR11",
                         url.download="http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11",
                         verbose=FALSE,...) {
  if(verbose) print("thredds.urls")
  url <- url.base
  if(!is.null(url.rel)) url <- paste(url,url.rel,sep="/")
  if(is.null(url.download)) url.download <- url.base
  if(!grepl(url,"catalog.html")) url <- paste(url,"catalog.html",sep="/")
  continue <- TRUE
  url.files <- NULL
  while(continue) {
    for(u in url) {
      txt <- readLines(u)
      txt <- txt[grep("href",txt)]
      files <- txt[grep(pattern,txt)]
      if(length(files)>0) {
        files <- gsub(paste("'>.*",sep=""),"",files)
        files <- gsub(paste(".*./",sep=""),"",files)
        u.data <- gsub(url.base,url.download,u)
        u.data <- gsub("catalog.html","",u.data)
        url.files <- c(url.files,paste(u.data,files,sep=""))
      }
      folders <- txt[grep("Folder",txt)]
      if(length(folders)>1) {
        folders <- gsub(".*href='|'>.*","",folders)
        url <- NULL
        for(i in seq(2,length(folders))) {
          u.i <- gsub("catalog.html",folders[i],u)
          url <- c(url,u.i)
        } 
      } else {
        continue <- FALSE
      }
    }
  }
  if(!is.null(select)) url.files <- url.files[select]
  return(url.files)
}

#CORDEXList <- c("http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1_day_19700101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1_day_19700101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_CLMcom-CCLM4-8-17_v1_day_19491201-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day_19510101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1_day_19700101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_day_19500101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_SMHI-RCA4_v1_day_19700101-20051230.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/tas/tas_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19491201-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19500101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_SMHI-RCA4_v1_day_19700101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_IPSL-IPSL-CM5A-MR_historical_r1i1p1_SMHI-RCA4_v1_day_19700101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_CLMcom-CCLM4-8-17_v1_day_19491201-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day_19510101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1_day_19700101-20051231.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_day_19500101-20051231.nc", "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_MOHC-HadGEM2-ES_historical_r1i1p1_SMHI-RCA4_v1_day_19700101-20051230.nc", 
#                "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/raw/pr/pr_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19491201-20051231.nc")
#
#CORDEXAdjList <- c("http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/adjusted/tas/hist/tasAdjust_EUR-11_ICHEC-EC-EARTH_rcp45_r12i1p1_SMHI-RCA4_v1-METNO-QMAP-MESAN-1989-2010_day_19700101-20051231.nc", 
#                   "http://thredds.met.no/thredds/dodsC/postclim/data/CORDEX-EUR11/adjusted/pr/hist/prAdjust_EUR-11_ICHEC-EC-EARTH_rcp45_r12i1p1_SMHI-RCA4_v1-METNO-QMAP-MESAN-1989-2010_day_19700101-20051231.nc")
