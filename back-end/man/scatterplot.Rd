\name{scatterplot}
\alias{scatterplot}
\title{Scatterplot}
\description{
Make a scatterplot
}
\usage{
scatterplot(x,y,ix=NULL,xlim=NULL,ylim=NULL,xlab=NULL,ylab=NULL,im=NULL,
                        main=NULL,legend=NULL,show.legend=TRUE,pal="cat",pch=21,cex=1.5,lwd=1.5,
                        new=FALSE,verbose=FALSE)
}
\arguments{
  \item{x}{a vector of same length as y}
  \item{y}{a vector of same length as x}
  \item{ix}{indices of points to emphasize with larger markers and darker color (NULL = none)}
  \item{xlim}{range of x axis}
  \item{ylim}{range of y axis}
  \item{xlab}{a title for the x axis}
  \item{ylab}{a title for the y axis}
  \item{im}{indices of points to include in scatterplot (NULL = all)}
  \item{main}{a main title for the plot}
  \item{legend}{text to add in legend}
  \item{show.legend}{a logical indicating if a legend should be included or not}
  \item{pal}{color palette}
  \item{pch}{either an integer specifying a symbol or a single character to be used in plotting points}
  \item{cex}{a numerical value giving the amount by which plotting text and symbols should be magnified relative to the default}
  \item{lwd}{a numerical value giving the amount by which the line width should be magnified relative to the default}
  \item{new}{A logical, defaulting to FALSE. If TRUE a new device is opened before the plot is produced.}
  \item{verbose}{A logical defaulting to FALSE. If TRUE, display information on progress.}
}
\keyword{scatterplot}