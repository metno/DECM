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
