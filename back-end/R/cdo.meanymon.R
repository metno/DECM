cdo.meanymon <- function(model.file,mask=NULL,seasonal=FALSE,
                         monthly=FALSE,is.temp=TRUE,out.file=NULL,verbose=FALSE) {
  
  commands <- c("-fldmean","-timmean")
  input <- c("","")
  
  if(!is.null(mask)){
    commands <- append(commands,"-maskregion",after=1)
    input <- append(input,mask,after=1) 
  }
  if(monthly) {
    commands <- replace(commands,commands=="-timmean","")
  } else if(seasonal){
    commands <- replace(commands,commands=="-timmean","-yseasmean")
  }
  
  if(is.null(out.file)) {
    out.file <- "tmp.nc"
    save.file <- FALSE
  } else {
    save.file <- TRUE
  }
  
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
  # If applying to e.g. slp data, set is.temp to FALSE to skip this correction:
  if(out>200 & is.temp) out <- out-273.15 
  if(!save.file) system(paste("rm",out.file,sep=" "))
  invisible(out)
}
