\name{cdo.command}
\alias{cdo.command}
\alias{cdo.mean}
\alias{cdo.spatSd}
\alias{cdo.timeSd}
\alias{cdo.gridcor}

\title{
   CDO calculations in R
}
\description{
  Do operations on netCDF files with CDO. More efficient than doing the calculations directly in R.
}
\usage{
cdo.command(commands,input,infile,outfile,intern=FALSE)
cdo.mean(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
         monthly=FALSE,is.temp=TRUE,outfile=NULL,verbose=FALSE)
cdo.spatSd(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
           monthly=FALSE,verbose=FALSE)
cdo.timeSd(model.file,period=c(1981,2010),mask=NULL,seasonal=FALSE,
           monthly=FALSE,verbose=FALSE)
cdo.gridcor(model.file,reference.file,period=c(1981,2010),mask=NULL,
            seasonal=FALSE,monthly=FALSE,verbose=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{commands}{the system commands to be invoked}
  \item{input}{input to the commands}
  \item{infile}{path to input netCDF file}
  \item{outfile}{path to output netCDF file}
  \item{intern}{A logical (not NA) which indicates whether to capture the output of the command as an R character vector.}
  \item{model.file}{path to netCDF file}
  \item{reference.file}{path to netCDF another file}
  \item{period}{a subset of years to include in calculations}
  \item{mask}{a spatial mask}
  \item{seasonal}{if TRUE return seasonal mean values}
  \item{monthly}{if TRUE return monthly mean values}
  \item{is.temp}{a logical indicating if the input is temperature data, in which case some corrections may be applied}
  \item{verbose}{if TRUE display information on progress}
}
\value{
\item{out}{A vector containing statistical values}
}
\author{
Olle Raty
}
\examples{
# Download reference data (ERAinterim temperature)
ref.file <- getReference("era", "tas")
#if(is.logical(ref.file)) {
#  getERA("tas")
#  ref.file <- getReference("era", "tas")
#}

# Set period
period <- c(1981,2010)

# Calculate annual mean spatial standard deviation
spsd <- cdo.spatSd(ref.file, period, monthly=FALSE)
tsd <- cdo.timeSd(ref.file, period, monthly=FALSE)

# Calculate monthly mean
meanval <- cdo.mean(ref.file, period, monthly=TRUE)

\dontrun{
# Download GCM data 
gcms <- getGCMs(select=1, varid="tas")
gcm.file <- gcms[[1]]$filename

# Calculate spatial correlation between GCM and reference data
cor <- cdo.gridcor(gcm.file, ref.file, period)

# Use cdo.command to calculate the monthly mean 
cdo.command(commands="-ymonmean", input="", 
            infile=ref.file, outfile="ref.monmean.nc")
}
}

\keyword{cdo}
\keyword{cdotools}