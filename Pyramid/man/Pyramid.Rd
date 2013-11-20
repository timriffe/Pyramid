\name{Pyramid}
\alias{Pyramid}
\title{a function for plotting population pyramids}
\usage{
  Pyramid(males, females, widths, prop = TRUE,
    standardize = TRUE, fill.males, fill.females,
    border.males = "transparent",
    border.females = "transparent", grid = TRUE,
    grid.lty = 2, grid.col = "grey", grid.lwd = 1,
    grid.bg = "transparent", coh.axis = FALSE,
    coh.lines = FALSE, year = 2000, coh.lty, coh.col,
    coh.lwd, coh.lines.min = FALSE, coh.at.min,
    coh.lty.min, coh.col.min, coh.lwd.min,
    age.lines = TRUE, age.lty, age.col, age.lwd,
    age.lines.min = FALSE, age.at.min, age.lty.min,
    age.col.min, age.lwd.min, v.lines = TRUE, v.lty, v.col,
    v.lwd, v.lines.min = FALSE, v.at.min, v.lty.min,
    v.col.min, v.lwd.min, main, xlim, ylim, cex.main = 1,
    cex.lab = 1, mar, xlab, ylab.left = "Age",
    ylab.right = "Cohort", xax.at, xax.lab, age.at,
    age.lab, coh.at, coh.labs, cex.axis = 1, box = TRUE,
    verbose = TRUE)
}
\arguments{
  \item{males}{either a numeric vector of male population
  counts by age or a matrix or data.frame of the male
  population, where each column is a state (e.g. employed,
  unemployed).}

  \item{females}{either a numeric vector of female
  population counts by age or a matrix or data.frame of the
  female population, where each column is a state (e.g.
  employed, unemployed).}

  \item{widths}{numeric vector of the age-interval widths;
  must be the same length as \code{males} and
  \code{females}. If missing, defaults to
  \code{rep(1,length(males))}. Population counts are always
  divided by the interval widths for plotting. This makes
  bar magnitudes comparable between pyramids with different
  age intervals; bars are to be interpreted as single year
  ages, where all of the single ages within a 5-year class
  have the same value. Ages for axes are computed from
  \code{widths}.}

  \item{prop}{logical. Should the x-axis be in percent or
  absolute value? Defaults to \code{TRUE}(percent axis). If
  absolute, the function tries to guess how many 0s to
  include in tick labels, and indicates millions or
  thousands in the axis labels.}

  \item{standardize}{logical. Default \code{TRUE}. Should
  bar volumes be made comparable with single-age pyramids
  (and hence with each other)? If \code{TRUE}, counts are
  divided by widths. Otherwise, counts are taken
  ``as-is''.}

  \item{fill.males}{(\code{fill.females}) The fill color
  for the male bars (left side) and female bars (right
  side). Can be specified in any way that R accepts.
  Defaults are \code{"orange"} and \code{"purple"},
  respectively, for simple pyramids, or \code{rainbow(k)}
  for multistate pyramids, where k is the number of states
  (columns in the input data).}

  \item{border.males}{(\code{border.females}) border color
  for the male and female bars. Can be specified in any way
  that R accepts. Default = \code{"transparent"}.}

  \item{grid}{logical. Defaults to \code{TRUE}. Should
  reference lines be drawn across the plot for age, cohort
  or x axis ticks?}

  \item{grid.lty}{(\code{grid.col, grid.lwd, grid.bg})
  graphical parameters for default reference lines. These
  get passed on to age, cohort and vertical reference lines
  unless specified specifically. \code{grid.lty} is
  analogous to \code{lty}, \code{grid.col} to \code{col}
  and so forth. Background color can be specified with
  \code{grid.bg}. Defaults are \code{2}, \code{"grey"},
  \code{1} and \code{"transparent"}, respectively.}

  \item{coh.axis}{logical. Defaults to \code{FALSE}. Should
  a cohort axis be drawn on the right side? In this case,
  you must also specify \code{year} to correctly compute
  birth cohorts. Ticks will be placed automatically on
  years ending in 0.}

  \item{coh.lines}{logical. Defaults to \code{FALSE}.
  Should cohort reference lines be drawn across the plot?
  If switched to \code{TRUE}, the lines will match tick
  mark locations (on 0s) and copy the graphical parameters
  from \code{grid.}.}

  \item{year}{The data year. This is only necessary if you
  want a cohort axis. Default value is 2000, so be
  careful!}

  \item{coh.lty}{(\code{coh.col, coh.lwd}) graphical
  parameters for cohort reference lines. Defaults to the
  \code{grid.} values.}

  \item{coh.lines.min}{logical. Defaults to \code{FALSE}.
  Should minor reference lines be drawnfor cohorts as
  well?}

  \item{coh.at.min}{optional vector of cohort values at
  which to draw minor reference lines. Defaults to cohort
  years evenly divisible by 5.}

  \item{coh.lty.min}{(\code{coh.col.min, coh.lwd.min})
  graphical parameters for minor cohort reference lines.
  Defaults are \code{3}, \code{coh.col} and \code{coh.lwd *
  .8}, respectively.}

  \item{age.lines}{logical. Defaults to \code{TRUE}. Should
  age reference lines be drawn across the plot? Defaults to
  the value of the age axis ticks and copies the graphical
  parameters from \code{grid.}.}

  \item{age.lty}{(\code{age.col, age.lwd}) optional
  graphical parameters to control the appearance of age
  grid reference lines. Defaults come from \code{grid.}}

  \item{age.lines.min}{logical. Defaults to \code{FALSE}.
  Should minor reference lines be drawn for ages as well?}

  \item{age.at.min}{optional vector of age values at which
  to draw minor reference lines.  Defaults to ages evenly
  divisible by 5.}

  \item{age.lty.min}{(\code{age.col.min, age.lwd.min})
  optional graphical parameters to control the appearance
  of minor age reference minor lines. Defaults come from
  \code{age.}}

  \item{v.lines}{logical. Defaults to \code{TRUE}. Should
  vertical reference lines be drawn across the plot?
  Defaults to the value of the x axis ticks and copies the
  graphical parameters from \code{grid.}.}

  \item{v.lty}{(\code{v.col, v.lwd}) optional graphical
  parameters to control the appearance of vertical grid
  reference lines. Defaults come from \code{grid.}}

  \item{v.lines.min}{logical. Defaults to \code{FALSE}.
  Should minor reference lines be drawn for vertical grid
  direction as well?}

  \item{v.at.min}{optional vector of x axis values at which
  to draw minor reference lines. Defaults to
  \code{pretty(xlim, n = 25, min.n = 16)}.}

  \item{v.lty.min}{(\code{v.col.min, v.lwd.min}) optional
  graphical parameters to control the appearance of minor
  vertical reference lines. Defaults come from \code{v.}}

  \item{main}{optional plot title, defaults to just telling
  you the total population. To not plot a title, simple
  specify \code{""}.}

  \item{cex.main}{character expansion of plot title.
  Defaults to 1, normal plot title size.}

  \item{xlim}{(\code{ylim}) x and y limits for the plot.
  Remember that even though axis labels for males on the
  left side are strictly positive, it is plotted in
  negative coordinate space}

  \item{mar}{plot margins. Same as \code{par("mar"}.
  Defaults to \code{c(5,5,5,5)}.}

  \item{xlab}{x axis label. Defaults to the most reasonable
  of \code{"percent"}, \code{"population (1000s)"} or
  \code{"population (millions)"}}

  \item{ylab.left}{left (age) axis label. Defaults to
  \code{"Age"}}

  \item{ylab.right}{right (cohort) axis label. Defaults to
  \code{"Cohort"}. Only plots if you specify \code{coh.axis
  = TRUE}.}

  \item{cex.lab}{character expansion of axis labels.
  Defaults to 1, normal \code{xlab} and \code{ylab} sizes.
  Applies to \code{xlab}, \code{ylab.right} and
  \code{ylab.left}.}

  \item{xax.at}{optional, values at which to draw x axis
  ticks. Default makes use of \code{pretty()}}

  \item{xax.lab}{optional, labels to print on x axis
  ticks.}

  \item{age.at}{optional, values at which to draw age
  (left) axis ticks. Defaults to multiples of 10.}

  \item{age.lab}{optional, labels to print on age axis
  ticks.}

  \item{coh.at}{optional, values at which to draw cohort
  (left) axis ticks. Defaults to multiples of 10}

  \item{coh.labs}{optional, labels to print on cohort axis
  ticks.}

  \item{cex.axis}{character expansion of all tick labels
  (x, age, and cohort).}

  \item{box}{logical. Defaults to \code{TRUE}. Draw box
  around plot area.}

  \item{verbose}{logical. Should informative but
  potentially annoying messages be returned when the
  function does something you might want to know about?
  Defaults to \code{TRUE}.}
}
\value{
  \code{NULL} function called for its side effects.
}
\description{
  The Pyramid package provides a simple wrapper to
  \code{barplot()} with several optional arguments and
  defaults to quickly plot a population pyramid, and with
  automatic detection and plotting of multistate pyramids.
  The function also gives optional absolute or percent
  scales, with flexible age-group widths, optional cohort
  labels on the right axis, and optional reference lines.
}
\details{
  In most cases, this function has all the options that you
  might need for a quick population pyramid, should allow
  for a wide variety of styles and be flexible to different
  age groupings. See examples below for a demonstration of
  features. If you really want to have full control over
  the design, look at the function code for ideas about the
  \code{barplot()} settings that are needed to get started.
}
\note{
  suggestions welcome
}
\examples{
data(PTpop)
   head(PTpop)

# default
   Pyramid(males=PTpop[,1], females=PTpop[,2])

# remove messages
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           verbose = FALSE)

# add cohort axis on right:
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           verbose = FALSE,
           coh.axis = TRUE)

# but watch out! it needs to know the data year to get the cohorts right! (assumes year 2000)
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           verbose = FALSE,
           coh.axis = TRUE,
           year = 1950)

# you can change gridline parameters using grid.lty, grid.col, grid.lwd, grid.bg
# or you can turn them off using grid = FALSE
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           coh.axis=TRUE,
           verbose = FALSE,
           grid.lty=1,
           grid.lwd=.5)

# for instance, something close to the ggplot2 aesthetic
# give bars a border using border.males and border.females
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           verbose = FALSE,
           coh.axis=TRUE,
           grid.lty=1,
           grid.lwd=.5,
           grid.bg = gray(.9),
           grid.col = "white",
           border.males="black",
           border.females="black")

# get axis labels, title using arguments main, xlab, ylab.left and ylab.right
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           coh.axis=TRUE,
           verbose = FALSE,
           grid.lty=1,
           grid.lwd=.5,
           grid.bg = gray(.9),
           grid.col = "white",
           border.males="black",
           border.females="black",
           xlab = "Porcentage",
           ylab.left = "Edad",
           ylab.right = "Cohorte",
           main = "Portugal Population Structure, 1950")

# change bar colors using fill.males and fill.females
# change x axis from percentage to absolute
# only vertical reference lines
# notice that the right ylab only shows if the axis is drawn:
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           verbose = FALSE,
           grid.lty=1,
           age.lines = FALSE,
           border.males="black",
           border.females="black",
           ylab.left = "Edad",
           ylab.right = "Cohorte",
           xlab = "poblaci\\'{o}n (1000s)",
           main = "Portugal Population Structure, 1950",
           fill.males = "orange",
           fill.females = "yellow",
           prop = FALSE)

# get age on both sides by tricking the function using
# coh.at and coh.labs (coh.at wants cohort positions, coh.labs in ths case are age labels)
# you can alter age, vertical and cohort lines independently, using
# v.col, v.lty, v.lwd, etc, coh._, age._
# here we explicitly leave the right axis label blank, otherwise 'cohort' will show up
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           verbose = FALSE,
           grid.lty=1,
           v.lty = 2,
           coh.axis = TRUE,
           border.males="black",
           border.females="black",
           xlab = "Poblaci\\'{o}n (1000s)",
           ylab.left = "Edad",
           ylab.right = "",
           main = "Portugal Population Structure, 1950",
           prop = FALSE,
           fill.males = "orange",
           fill.females = "yellow",
           coh.at = 1950 - seq(0,100,by = 10),
           coh.labs = seq(0,100, by = 10)
   )

# get vertical lines every .05, but only labeled every .2 (with prop = TRUE)

# coh., v. and age. graphical parameters can also refer to minor lines using the .min suffix,
# e.g. v.lty becomes v.lty.min
# we can turn on automatic minor lines using
# v.lines.min = TRUE, age.lines.min = TRUE, coh.lines.min = TRUE

# also, you can give explicit character string labels for the x axis
# (not for y axes, they must be numeric, sorry)- in this case some nice percents
# good idea to give explicit locations using xax.at

# made cex.axis smaller so labels would fit well
   xlabats <- seq(from = -1.2,to = 1.2, by = .2)
   xlabs <- c("1.2\\\%","1.0\\\%","0.8\\\%","0.6\\\%","0.4\\\%","0.2\\\%","0.0\\\%","0.2\\\%","0.4\\\%","0.6\\\%","0.8\\\%","1.0\\\%","1.2\\\%")

   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           verbose = FALSE,
           grid.lty=1,
           v.lty = 2,
           coh.axis = TRUE,
           coh.lines = FALSE,
           border.males="black",
           border.females="black",
           xlab = "Porcentage",
           ylab.left = "Edad",
           ylab.right = "",
           main = "Portugal Population Structure, 1950",
           fill.males = "orange",
           fill.females = "yellow",
           coh.at = 1950 - seq(0,100,by = 10),
           coh.labs = seq(0,100, by = 10),
           xax.at = xlabats,
           xax.lab = xlabs,
           v.lines.min = TRUE,
           cex.axis=.75
   )


# specify some custom minor tick marks for age
# (change to taste!)
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           verbose = FALSE,
           coh.lines = FALSE,
           coh.axis = TRUE,
           year = 1950,
           border.males="black",
           border.females="black",
           xlab = "Porcentage",
           ylab.left = "Edad",
           ylab.right = "",
           main = "Portugal Population Structure, 1950",
           fill.males = "orange",
           fill.females = "yellow",
           coh.at = 1950 - seq(0,100,by = 10),
           coh.labs = seq(0,100, by = 10),
           xax.at = xlabats,
           xax.lab = c("1.2\\\%","1.0\\\%","0.8\\\%","0.6\\\%","0.4\\\%","0.2\\\%","0.0\\\%","0.2\\\%","0.4\\\%","0.6\\\%","0.8\\\%","1.0\\\%","1.2\\\%"),
           cex.axis=.75,
           age.lines.min = TRUE,
           age.at.min = seq(0,100,by = 2),
           age.col.min = gray(.7),
           age.lty.min = 3,
           age.lwd.min = .7
   )


# plot a transparent pyramid on top of another pyramid:
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           verbose = FALSE,
           grid.lty=1,
           coh.lines = FALSE,
           xlab = "Porcentage",
           ylab.left = "Edad",
           ylab.right = "",
           main = "Portugal Population Structure, 1950 & 2001",
           fill.males = "orange",
           fill.females = "yellow",
           border.males="black",
           border.females="black",
           coh.axis = TRUE,
           coh.at = 1950 - seq(0,100,by = 10),
           coh.labs = seq(0,100, by = 10),
           xax.at = xlabats,
           xax.lab = c("1.2\\\%","1.0\\\%","0.8\\\%","0.6\\\%","0.4\\\%","0.2\\\%","0.0\\\%","0.2\\\%","0.4\\\%","0.6\\\%","0.8\\\%","1.0\\\%","1.2\\\%"),
           cex.axis=.75
   )
# use par(new=TRUE) to allow plotting on top
# turn off several parameters, like grid, labels, make fill explicitly transparent (or NA)

# be carefully to explicitly define xlim and ylim for both plots so they match dimensions perfectly
   par(new=TRUE)
   Pyramid(males = PTpop[,3],
           females = PTpop[,4],
           verbose = FALSE,
           grid=FALSE,
           border.males="black",
           border.females="black",
           xlab = "",
           ylab.left = "",
           ylab.right = "",
           main = "",
           fill.males = NA,
           fill.females = NA,
           xlim=c(-1.2,1.2),
           ylim=c(0,101),
           xax.at = xlabats,
           xax.lab = c("1.2\\\%","1.0\\\%","0.8\\\%","0.6\\\%","0.4\\\%","0.2\\\%","0.0\\\%","0.2\\\%","0.4\\\%","0.6\\\%","0.8\\\%","1.0\\\%","1.2\\\%"),
           cex.axis=.75,
           coh.axis = FALSE
   )

   ## or a semi-transparent pyramid where you can see the grid lines behind it:

# define transparency function for named colors:
   colalpha <- function(color,alpha){
       colalphai <- function(color,alpha){
          paste(rgb(t(col2rgb(color)/255)),alpha,sep="")
       }
       sapply(color,colalphai,alpha=alpha)
   }

   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           verbose = FALSE,
           grid.lty = 1,
           grid.col = gray(.7),
           coh.lines = FALSE,
           xlab = "Porcentage",
           ylab.left = "Edad",
           ylab.right = "",
           main = "Portugal Population Structure, 1950",
           fill.males = colalpha("orange",90),
           fill.females = colalpha("yellow",90),
           border.males="black",
           border.females="black",
           coh.axis = TRUE,
           coh.at = 1950 - seq(0,100,by = 10),
           coh.labs = seq(0,100, by = 10),
           xax.at = xlabats,
           xax.lab = c("1.2\\\%","1.0\\\%","0.8\\\%","0.6\\\%","0.4\\\%","0.2\\\%","0.0\\\%","0.2\\\%","0.4\\\%","0.6\\\%","0.8\\\%","1.0\\\%","1.2\\\%"),
           cex.axis = .75,
           # minor grid line arguments:
           v.lines.min = TRUE,
           v.lty.min = 1,
           v.lwd.min = .7,
           v.col = "black"
   )


# another option would be to simply draw the reference lines on top of the pyramid:

# specify grid = FALSE
# then draw manually
   Pyramid(males = PTpop[,1],
           females = PTpop[,2],
           year = 1950,
           verbose = FALSE,
           grid = FALSE,
           xlab = "Porcentage",
           ylab.left = "Edad",
           ylab.right = "",
           main = "Portugal Population Structure, 1950",
           fill.males = "orange",
           fill.females = "yellow",
           coh.axis = TRUE,
           coh.at = 1950 - seq(0,100,by = 10),
           coh.labs = seq(0,100, by = 10),
           xax.at = xlabats,
           xax.lab = c("1.2\\\%","1.0\\\%","0.8\\\%","0.6\\\%","0.4\\\%","0.2\\\%","0.0\\\%","0.2\\\%","0.4\\\%","0.6\\\%","0.8\\\%","1.0\\\%","1.2\\\%"),
           cex.axis=.75,
   )
# plot minor reference lines
   segments(-1.2,seq(2,100,by=2),1.2,seq(2,100,by=2), col = colalpha(gray(.2),50), lwd = .8, lty = 3)
   segments(seq(-1.2,1.2,by=.1),0,seq(-1.2,1.2,by=.1), 101, col = colalpha(gray(.2),50), lwd = .8, lty = 3)
# plot major reference lines
   segments(-1.2,0:10 * 10,1.2, 0:10 *10, col = colalpha(gray(.4),50),lwd=.8)
   segments(seq(-1.2,1.2,by=.2),0,seq(-1.2,1.2,by=.2), 101, col = colalpha(gray(.4),50),lwd=.8)

# example of a multistate pyramid.
   data(ESextr)
   males <- matrix(as.numeric(ESextr[1:18,2:5]),ncol=4)
   females <- matrix(as.numeric(ESextr[19:36,2:5]),ncol=4)
   widths <- rep(5,nrow(ESextr)/2)

   Pyramid(males,
           females,
           widths=widths,
           main = "Foreign population in Spain  (registered) by continent of bith, 2010",
           fill.males=gray(c(.8,.6,.4,.2)),
           fill.females=gray(c(.8,.6,.4,.2))
   )
   rect(1,70,2,90,col="white",border="black")
   legend(1,90,legend=c("Europe","Africa","Americas","Asia"),fill=gray(c(.8,.6,.4,.2)),xpd=TRUE,bty="n")

# goes well together with individual subgroup plots.
# remember, specify equal x axes for all of them:
# african males force us to bring the axes out to 3!
   countries <- c("Europe","Africa","Americas","Asia")
   dev.new(height=8,width=8)
   par(mfrow=c(2,2))
   for (i in 1:4){
       Pyramid(males[,i],
               females[,i],
               widths=widths,
               main = countries[i],
               fill.males=gray(.4),
               fill.females=gray(.6),
               xlim=c(-3,3),
               mar=c(2,2,2,2)
       )
   }
}
\author{
  Tim Riffe \email{tim.riffe@gmail.com}
}
\seealso{
  See also the \code{pyramid} package on CRAN, which plots
  the age axis in the middle.
}
\keyword{hplot}

