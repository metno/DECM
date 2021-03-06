\name{calculate.statistics}
\alias{calculate.statistics.cmip}
\alias{calculate.statistics.cordex}
\alias{calculate.rmse.cmip}
\alias{calculate.rmse.cordex}
\alias{calculate.mon.weights}
\title{Calculate statistics}
\description{
\code{calculate.statistics.cmip} calculates the annual and monthly standard deviation and mean values of GCM output (CMIP5 data). If a reference data set is specified and the selected period is in the past, the spatial correlation between the reference data and the GCM output is also calculated. The calculations are done both for the whole globe and for the 26 SREX regions. 
\code{calculate.statistics.cordex} calculates the same statistics but for RCM output (CORDEX data), using the EOBS dataset as a reference and the PRUDENCE regions in Europe.
\code{calculate.rmse.cmip} and \code{calculate.rmse.cordex} calculate the root mean square error (RMSE) of the seasonal cycle compared to the reference data.
\code{calculate.mon.weights} calculates weights based on the latitude and number of days per month that are used in \code{calculate.rmse.cmip} and \code{calculate.rmse.cordex}.
}
\usage{
calculate.statistics.cmip(reference="era", period=c(1981,2010), variable="tas", 
                          experiment="rcp45", nfiles=5, path.gcm=NULL, continue=TRUE, 
                          mask="coords.txt", verbose=FALSE)
calculate.statistics.cordex(reference="eobs", period=c(1981,2010), 
                            variable="tas", path.rcm=NULL, continue=TRUE, 
                            mask="PrudenceCoords.txt",
                            experiment="rcp45", verbose=FALSE)
calculate.rmse.cmip(reference="era", period=c(1981,2010), variable="tas", experiment="rcp45", 
                    nfiles=4, continue=TRUE, verbose=FALSE, path=NULL, path.gcm=NULL)
calculate.rmse.cordex(reference="eobs", period=c(1981,2010), variable="tas", 
                      nfiles=4, continue=TRUE, path=NULL, path.gcm=NULL,
                      experiment="rcp45", verbose=FALSE)
calculate.mon.weights(lon,lat)
}
\arguments{
  \item{reference}{reference data set: "era" (ERAinterim), "eobs", or "cfsr")}
  \item{period}{a subset of years to include in calculations}
  \item{variable}{variable id: 'tas' = temperature, 'pr' = precipitation}
  \item{experiment}{parent experiment of GCM data, e.g Representative Concentration Pathway RCP4.5 ('rcp45') or RCP8.5 ('rcp85')}
  \item{nfiles}{number of files to analyse, can be a number or "all"}
  \item{path.gcm}{path to GCM data}
  \item{path.rcm}{path to RCM data}
  \item{path}{path to output files}
  \item{continue}{if TRUE add statistics to existing file, if FALSE create new file}
  \item{mask}{a spatial mask}
  \item{lon}{a vector containing longitudes}
  \item{lat}{a vector containing latitudes}
  \item{verbose}{if TRUE display information on progress, if FALSE run in silent mode}
}
\examples{
\dontrun{
calculate.statistics.cmip(reference=NULL, period=c(2071,2100), 
          variable="tas", nfiles=5, continue=TRUE,
				  mask=NULL, experiment="rcp45", verbose=TRUE)
}
}
\keyword{statistics}
\keyword{rmse}