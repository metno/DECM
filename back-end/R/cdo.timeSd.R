## Calculate the temporal standard deviation with cdo
cdo.timeSd <- function(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
                       monthly=FALSE,verbose=FALSE) {
  if(verbose) print("cdo.timeSd")
  commands <- c("-timstd","-fldmean","-yearmean","-selyear")
  input <- c("","","",paste(period,collapse="/"))
  
  if(!is.null(mask)) {
    commands <- append(commands,"-maskregion",after=3)
    input <- append(input,mask,after=3) 
  }
  if(monthly) {
    commands <- replace(commands,commands=="-yearmean","-monmean")
  } else if(seasonal) {
    commands <- replace(commands,commands=="-yearmean","-seasmean")
  }
  
  out.file <- "tmp.nc"
  cdo.command(commands,input,model.file,out.file)
  
  command <- ("output")
  input <- c("")
  
  out <- as.numeric(cdo.command(command,input,out.file,NULL,intern=TRUE))
  if(monthly) {
    names(out) <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  } else if(seasonal) {
    names(out) <- c("djf","mam","jja","son")
  } else {
    names(out) <- "ann"
  }
  system(paste("rm",out.file,sep=" "))
  invisible(out)
}
