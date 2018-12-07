# Call to a python script which downloads data from the public ECMWF data server
python.getEra <- function(start,end,variable,steps,type,stream,outfile,verbose=FALSE) {
  if(verbose) print("python.getEra")
  #script <- "python python/getMonthlyERA.py"
  path.era <- find.file("getMonthlyERA.py")
  script <- paste("python",path.era[1])
  if(verbose) print("Run ECMWF Python script for downloading ERA data")
  system.command <- paste(script," -f ",start," -l ",end," -v ",variable,
                          " -s ",steps," -t ",type," -r ",stream," -o ",outfile, sep="")
  system(system.command,wait=TRUE)
  if(verbose) print("--- end python.getEra")
}
