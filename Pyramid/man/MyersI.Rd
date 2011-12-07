\name{MyersI}
\alias{MyersI}

\title{
Calculate Myers' blended index for age heaping based on digit preference.
}
\description{
Calculate Myers' blended index for age heaping based on digit preference. Implemented as described in Shyrock et. al. (1980).
}
\usage{
MyersI(x, ages)
}

\arguments{
  \item{x}{
vector of age-specific population counts
}
  \item{ages}{
vector of completed ages.
}
}
\details{
Large numbers mean more bias in digit preference. Numbers less than 1 can be disregarded. Interpret as percent of population that would need to be moved to a different age category for there to be no noticeable pattern of preference in age declaration. 
}
\value{
the blended index value is returned. 
}
\references{
Myers, R.J. (1940) Errors and bias in the reporting of ages in census data. Transactions of the Actuarial Society of America. vol 41 nr 2 pages 395--415\\
Shryock, H.S. and Siegel, J.S. and Larmon, E.A. (1980) The methods and materials of demography. US Dept. of Commerce, Bureau of the Census (pages 116-118)
}
\author{
Tim Riffe
}
\note{
This implementation will only work for digits 0-9: not months, zodiacs or anything else that doesn't have a period of 10
}
\examples{
data(PTpop)

# separately for males and females in both years
apply(PTpop,2,MyersI)

# males and females together, 1950
MyersI(rowSums(PTpop[,1:2]))
}
