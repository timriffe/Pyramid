\name{Pyramid-package}
\alias{Pyramid-package}
\alias{Pyramid}
\docType{package}
\title{Function and example data for plotting population pyramids.}
\description{
Population pyramids can be plotted in R by using the base R graphics function \code{barplot()}. The Pyramid package provides a simple wrapper with several defaults to quickly plot a population pyramid, and with simple detection and plotting of multistate pyramids. The function also gives optional absolute or percent scales, with flexible age-group widths, and optional generation labels on the right axis. Two example datasets from the Human Mortality Database are given to demonstrate plotting with single-age versus grouped ages.}
\details{
\tabular{ll}{
Package: \tab Pyramid\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2011-07-11\cr
License: \tab General Public\cr
LazyLoad: \tab yes\cr
}}
\author{
Tim Riffe\\

Maintainer: Tim Riffe <tim.riffe@gmail.com>
}
\references{
Human Mortality Database. University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany). Available at www.mortality.org or www.humanmortality.de (data downloaded on 10 May, 2010). 
}
\keyword{ package }
\keyword{ hplot }
\examples{
library(Pyramid)
data(ESP2000)
data(ESP2000_5)
par(mfrow=c(1,2))
Pyramid(males=ESP2000[,1],females=ESP2000[,2],prop=T,year=2000,
		generations=T,border.males="black",border.females="black",verbose=T)
Pyramid(males=ESP2000_5[,1],females=ESP2000_5[,2],widths=c(1,4,rep(5,(nrow(ESP2000_5)-2))),prop=T,year=2000,
		generations=T,border.males="black",border.females="black",verbose=F)

}
