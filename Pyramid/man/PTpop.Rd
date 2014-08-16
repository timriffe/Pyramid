\docType{data}
\name{PTpop}
\alias{PTpop}
\title{Portugal population counts by age and sex, years 1950 and 2001.}
\format{A data frame with 101 observations on the following 4 variables.
\describe{ \item{list("males_1950")}{a numeric vector}
\item{list("females_1950")}{a numeric vector} \item{list("males_2001")}{a
numeric vector} \item{list("females_2001")}{a numeric vector} }}
\source{
  Data aggregated from Portuguese census microdata from:
  Instituto Nacional de Estatística (INE).
  http://www.ine.pt.
}
\description{
  Portugal population counts by age and sex, years 1950 and
  2001. Single ages.
}
\details{
  ages 0 to 100.
}
\examples{
data(PTpop)
head(PTpop)

Pyramid(PTpop[,1],PTpop[,2])

# check for digit preference / age heaping. Higher = more heaping.
apply(PTpop,2,MyersI)
}
\references{
  Available in: Dinâmicas Demográficas e Envelhecimento da
  População Portuguesa: Evolução e Perspectivas. Instituto
  do Envelhecimento, Universidade de Lisboa.
}
\keyword{datasets}

