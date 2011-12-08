\name{PTpop}
\alias{PTpop}
\docType{data}
\title{
Portugal population counts by age and sex, years 1950 and 2001.
}
\description{
Portugal population counts by age and sex, years 1950 and 2001. Single ages.
}
\usage{data(PTpop)}
\format{
  A data frame with 101 observations on the following 4 variables.
  \describe{
    \item{\code{males_1950}}{a numeric vector}
    \item{\code{females_1950}}{a numeric vector}
    \item{\code{males_2001}}{a numeric vector}
    \item{\code{females_2001}}{a numeric vector}
  }
}
\details{
ages 0 to 100.
}
\source{
Data aggregated from Portuguese census microdata from: Instituto Nacional de Estatística (INE). http://www.ine.pt.
}
\references{
Available in: Dinâmicas Demográficas e Envelhecimento da População Portuguesa: Evolução e Perspectivas. Instituto do Envelhecimento, Universidade de Lisboa.
}
\examples{
data(PTpop)
head(PTpop)

Pyramid(PTpop[,1],PTpop[,2])

# check for digit preference / age heaping. Higher = more heaping.
apply(PTpop,2,MyersI)

}
\keyword{datasets}
