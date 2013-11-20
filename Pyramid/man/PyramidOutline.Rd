\name{PyramidOutline}
\alias{PyramidOutline}
\title{PyramidOutline draws a population pyramid as a single polygon}
\usage{
  PyramidOutline(males, females,
    scale = sum(c(males, females)), x = 0, y = 0, ...)
}
\arguments{
  \item{males}{vector of male population counts or
  fractions}

  \item{females}{vector of male population counts or
  fractions}

  \item{scale}{the total population, for purposes of
  plotting. For proportions in each age group, set to 1.}

  \item{x}{the x position of the middle of the pyramid}

  \item{y}{the y position of the bottom of the pyramid}

  \item{...}{arguments passed to \code{polygon()}.}
}
\value{
  function called primarily for its graphical side effects,
  although a list of x and y coordinates is invisibly
  returned.
}
\description{
  This function rescales and positions a population pyramid
  according to the convention of having males on the left
  and females on the right, and using discrete age groups.
  Ages are assumed to be in single-years intervals and to
  start at age 0. A plot device must already be open.
}

