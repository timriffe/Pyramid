\name{PTpop}
\alias{PTpop}
\docType{data}
\title{
Portugal population counts by age and sex, years 1950 and 2001.
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
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
Dinâmicas Demográficas e Envelhecimento da População Portuguesa: Evolução e Perspectivas. Instituto do Envelhecimento, Universidade de Lisboa. data aggregated from census microdata.
}
\references{
Instituto Nacional de Estatística (INE): http://www.ine.pt.
}
\examples{
data(PTpop)
head(PTpop)

Pyramid(PTpop[,1],PTpop[,2])

# check for digit preference / age heaping. Higher = more heaping.
apply(PTpop,2,MyersI)

}
\keyword{datasets}
