\name{aggN}
\alias{aggN}
\title{aggN aggregate vector of single age (year) data to N-year groups}
\usage{
  aggN(x, N)
}
\arguments{
  \item{x}{the vector of single-age-classified data}

  \item{N}{desired width of interval, e.g., 5 or 10}
}
\description{
  This only makes sense if N is 5 or 10 (or 2 I guess),
  since it uses modulo to find groups Ages are assumed to
  start at 0 and count up in single ages.
}

