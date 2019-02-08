\name{getPrudenceCoords}
\alias{getPrudenceCoords}
\title{Get coordinates of PRUDENCE regions}
\description{
Get coordinates of a chosen prudence region
}
\usage{
getPrudenceCoords(prudence,region,destfile="coords.txt")
}
\arguments{
  \item{prudence}{output filename}
  \item{region}{name of PRUDENCE region}
  \item{destfile}{name of output file}
}
\examples{
f <- find.file("RegionSpecifications.csv")
prudence <- read.csv(f[[1]])
getPrudenceCoords(prudence, prudence$Region[[1]], "coords.txt")
x <- read.table("coords.txt")
print(x)
}
\keyword{prudence}
\keyword{coordinates}