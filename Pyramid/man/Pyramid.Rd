\name{Pyramid}
\alias{Pyramid}
\title{
a function for plotting population pyramids
}
\description{
Population pyramids can be plotted in R by using the base R graphics function \code{barplot()}. The Pyramid package provides a simple wrapper with several optional arguments with defaults to quickly plot a population pyramid, and with simple detection and plotting of multistate pyramids. The function also gives optional absolute or percent scales, with flexible age-group widths, and optional generation labels on the right axis. Two example datasets from the Human Mortality Database are given to demonstrate plotting with single-age versus grouped ages.
}
\usage{
Pyramid(males, females, widths, prop = T, fill.males, fill.females, border.males = "transparent", border.females = "transparent", generations = T, year = 2000, gen.lty = 2, gen.col = "black", gen.lwd = 1, verbose = T, main, xlim, ylim, cex.main = 1, cex.lab = 1, mar, xlab, xaxat, xax.lab, yaxat, yax.lab)
}

\arguments{
  \item{males}{
either numeric vector of the age-specific male population or a matrix or data.frame of the male population, where each column is a state (e.g. employed, unemployed). Ages can be grouped in any way or single.
}
  \item{females}{
either numeric vector of the age-specific female population or a matrix or data.frame of the female population, where each column is a state (e.g. employed, unemployed). Ages can be grouped in any way or single.
}
  \item{widths}{
numeric vector of the age-interval widths; must be the same length as \code{males} and \code{females}. If missing, defaults to \code{rep(1,length(males)). Population counts are always divided by the interval widths for plotting. This makes bar magnitudes comparable between pyramids with different age intervals; bars are to be interpreted as single year ages, where all of the single ages within a 5-year class have the same value.}
}
  \item{prop}{
logical. Should the x-axis be in percent or absolute value? Default = TRUE = percent axis. If absolute, the function tries to guess how many 0s to include, and indicates millions or thousands in the axis label.
}
  \item{fill.males}{
The fill color for the male bars (left side). Can be specified in any way that R accepts. Default = \code{"orange"} for simple pyramids, or \code{rainbow(k)} for multistate pyramids, where k is the number of states (columns in the input data).
}
  \item{fill.females}{
The fill color for the female bars (left side). Can be specified in any way that R accepts. Default = \code{"purple"} for simple pyramids, or \code{rainbow(k)} for multistate pyramids, where k is the number of states (columns in the input data).
}
  \item{border.males}{
border color for the male bars. Can be specified in any way that R accepts. Default = \code{"transparent"}.
}
  \item{border.females}{
border color for the female bars. Can be specified in any way that R accepts. Default = \code{"transparent"}.
}
  \item{generations}{
logical. Should generation labels be included in the right margin? Default = TRUE. In this case, you need to specify the data year. This is calculated using the \code{widths} and \code{year} information, so if one of these is in error, the generation labels will also be in error.
}
  \item{year}{
data year. Only needs to be specified if generation labels are used. Also appears in the default plot title. Default = 2000.
}
  \item{gen.lty}{
line type for the generation lines (lines that trace in from the labels to point to the respective spot on the pyramid). If you don't want them plotted, then specify \code{gen.col} as \code{"transparent"}. These lines will only be plotted if \code{generations=TRUE}.
}
  \item{gen.col}{
the line color for the generation lines. Default = \code{"black"}. To turn off generation lines, specify this as as \code{"transparent"}. These will only be plotted if \code{generations=TRUE}.
}
  \item{gen.lwd}{
the line width for the generation lines. Default = 1.
}
  \item{verbose}{
logical. Should informative but potentially annoying messages be returned when the function does something you might want to know about? Default = \code{TRUE}.
}
  \item{main}{
The plot title. The default is something like "2000, Total Population 5 million", where the year is taken from the \code{year} argument (mostly to remind users to change this to properly set the generations labels), and millions or thousands rounding happens within the function.
}
  \item{xlim}{
optional specification of x-axis limits (useful for plotting several pyramids, either in an animated time series, or comparing populations). Otherwise these are done using \code{pretty}.
}
  \item{ylim}{
optional specification of y-axis limits, potentially useful for looking only at a particular age-range, though this does not always come out pretty. Also, beware that if axes are in percent scale and the visible age-range is limited, the percent labels refer to the whole population and not only the visible part of the population. Better for viewing a particular age range would be to only specify counts from the desired age range, and to use the \code{widths}, \code{yaxat} and \code{yax.labs} arguments to adjust the labels.
}
  \item{cex.main}{
Character expansion factor for the plot title. Default = 1.
}
  \item{cex.lab}{
Character expansion factor for label font size. default = 1. This applies to left, lower and right axes.
}
  \item{mar}{
plot margins. Default = \code{c(5,4,7,4) + 0.1}, which leaves space for a plot title and for generations labels.
}
  \item{xlab}{
label for x axis, defaults to "percent", "population (1000s)" or "population (millions)", depending on \code{prop} and the counts themselves.
}
  \item{xaxat}{
numeric vector. optional positioning of x axis labels. Remember that the left side of the pyramid is in negative space.
}
  \item{xax.lab}{
numeric vector. optional x axis labels. Remember that while the left bars plot to the left because they're negative, the labels still need to be positive.
}
  \item{yaxat}{
numeric vector. optional positioning of left y axis (Age) labels. Defaults to every 10 years of age.
}
  \item{yax.lab}{
numeric vector. optional left y axis (Age) labels. Defaults to every 10 years of age.
}
}
\details{
In most cases, this function has all the options that you might need for a quick population pyramid. If you really want to have full control over the design, look at the function code below for ideas about the \code{barplot()} settings that are needed to get started. Even more flexibility could be gained by plotting manually (and possibly much more laboriously) via the \code{polygon()} function.
}
\author{
Tim Riffe
}
\seealso{
There is a package posted on CRAN called "pyramid", with a different plotting strategy: see  \code{\link{pyramid}}
}
\examples{
library(Pyramid)
# some single year data included:
head(ESP2000)
# default settings:
Pyramid(ESP2000[,"males"],ESP2000[,"females"])
# change bar borders
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1)
# remove generations
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1,generations=F)
#get rid of annoying console message (could also just specify widths
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1,generations=F,verbose=F)
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1,generations=F,widths=rep(1,111))
# change title from default
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1,generations=F,verbose=F,main="Spain population structure, 2000")
# a decent looking pyramid, there are other arguments
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1,generations=F,verbose=F,main="Spain population structure, 2000",prop=F,cex.lab=1.3,cex.main=1.5)
legend("topright",fill=c("orange","purple"),legend=c("males","females"))

# 5-year abridged age groups:
head(ESP2000_5)
# now we need to specify widths:
(widths <- c(1,4,rep(5,(nrow(ESP2000_5)-2))))
Pyramid(ESP2000_5[,"males"],ESP2000_5[,"females"],widths=widths,border.males=1,border.females=1,generations=F,main="Spain population structure, 2000",prop=F,cex.lab=1.3,cex.main=1.5)
legend("topright",fill=c("orange","purple"),legend=c("males","females"))

# no title, different margins
Pyramid(ESP2000_5[,"males"],ESP2000_5[,"females"],widths=widths,border.males=1,border.females=1,generations=F,main="",prop=F,cex.lab=1.3,cex.main=1.5,mar=c(5,5,2,2))
legend("topright",fill=c("orange","purple"),legend=c("males","females"))

# bars are always to be interpreted as single years, making pyramids with different age intervals comparable:
windows(height=7,width=12)
par(mfrow=c(1,2))
Pyramid(ESP2000[,"males"],ESP2000[,"females"],border.males=1,border.females=1,generations=F,main="",cex.lab=1.3,cex.main=1.5,mar=c(5,5,2,2))
legend("topright",fill=c("orange","purple"),legend=c("males","females"))
Pyramid(ESP2000_5[,"males"],ESP2000_5[,"females"],widths=widths,border.males=1,border.females=1,generations=F,main="",cex.lab=1.3,cex.main=1.5,mar=c(5,5,2,2))
legend("topright",fill=c("orange","purple"),legend=c("males","females"))
# in the case of the pyramid on the right, values have been divided by the respective interval widths.
# this needs to be taken into account when judging both percents and absolute counts when ages are grouped.


## The function is currently defined as

	function(males,females,widths,prop=T,
		fill.males,fill.females,border.males="transparent",border.females="transparent",
		generations=T,year=2000,gen.lty=2,gen.col="black",gen.lwd=1,
		verbose=T,main,xlim,ylim,cex.main=1,cex.lab=1,mar,xlab,xaxat,xax.lab,yaxat,yax.lab){
	
	# short verbose code
	Verb <- function(v,x){
		if (v==TRUE) {
			cat(paste(x,"\n",sep=""))
		}
	}
	
	# check that male and female data are of equal length 
	if (length(males)!=length(females)) {stop("the vectors input for males and females are not of the same length\nthis function is too dumb to know what you want. sorry")}
	
	# for simple pyramids, k = 1; for multistate pyramids, we need a separate column for each of k states
	if (is.null(dim(males))){
		k <- 1
	} else { k <- ncol(males) }
	
	# we just assume 1-year age groups unless widths are specified, the user ought to immediately notice if something is wrong here...
	if (missing(widths)) {
		widths <- rep(1,(length(males)/k))
		Verb(verbose,"widths unspecified; assumed 1-year age intervals; this affects the pyramid dimensions and ages")
	}
	
	# in either case, we deduce ages from the age interval widths.
	ages <- cumsum(widths)-widths
	
	# total population 
	tot <- sum(males)+sum(females) 
	
	# for figuring out axes
	xmax1 <- if (is.null(dim(males))) {max(c(males,females)/widths)} else {max(c(rowSums(males),rowSums(females)/widths))}
	
	# should we round to 1000s or 1000000s for axis labels?
	KorM <- 10^(3*floor(log10(xmax1)/3))
	KorMTitle <- (10^(3*floor(log10(tot)/3)))
	KorMlab <- if (KorMTitle==1000) "(1000s)" else "(millions)"
	
	# deciding proper x axes and labels in accordance with prop:
	if (prop==TRUE){
		males <- (males/tot * -100)/widths # percent males (so that they plot to the left)
		females <- (females/tot * 100)/widths # percent females, plotting to the right
		if (is.null(dim(males))) {
			xmax <- max(c(-males,females))
		} else {
			xmax <- max(c(rowSums(-males),rowSums(females)))
			males <- t(males)
			females <- t(females)
		}
		if (missing(xlab)) {xlab <- "percent"}
	}
	if (prop==FALSE){
		males <- (-males/KorM)/widths
		females <- (females/KorM)/widths
		xmax <- if (is.null(dim(males))) {max(c(-males,females))} else {max(c(rowSums(-males),rowSums(females)))}
		if (missing(xlab)) {xlab <- paste("population"," (",KorM,"s)",sep="")}
	}
	
	# scipen=6 makes sure the labels aren't in scientific notation
	options(scipen=6)
	# age labels, percent or count labels
	if (missing(xaxat)) 		{xaxat 	<- pretty(c(-xmax,xmax),8)}
	if (missing(xax.lab)) 		{xax.lab 	<- abs(xaxat)}
	
	# age labels
	if (missing(yaxat)) 		{yaxat <- ages[ages\%\%10==0]}
	if (missing(yax.lab)) 		{yax.lab <- yaxat}
	
	# plotting limits
	if (missing(ylim)) 			{ylim <- range(ages)}
	if (missing(xlim)) 			{xlim <- range(xaxat)}
	
	# some default colors
	# k > 1 identifies multistate pyramids, for which colors go left to right along columns in the input data, in to out in the pyramid itself
	if (k > 1){
		if (missing(fill.males)) 		{fill.males <- rainbow(k)}
		if (missing(fill.females)) 		{fill.females <- rainbow(k)}
		if (missing(border.males)) 		{border.males <- "black"}
		if (missing(border.females)) 	{border.females <- "black"}
	}
	if (k==1){
		if (missing(fill.males)) 		{fill.males <- "orange"}
		if (missing(fill.females)) 		{fill.females <- "purple"}
		if (missing(border.males)) 		{border.males <- "transparent"}
		if (missing(border.females)) 	{border.females <- "transparent"}
	}
	
	# a default title, if year is not specified then 2000 is used, and the user ought to notice. This is so that
	# the user doesn't accidentally allow the generations to be calculated using the year 2000.
	if (missing(main)){ main <- paste(year,",Total Pop = ",round(tot/KorMTitle,digits=1),KorMlab)}
	
	# default margins
	if (missing(mar)){ mar <- c(5,4,7,4) + 0.1}
	
	par(mar=mar)
	# pyramid, left and right plotted separately
	barplot(males,width=widths,horiz=T,space=0,xlim=xlim,ylim=ylim,ylab="Age",xlab=xlab,border=border.males,
			main=main,axes=F,cex.main=cex.main,xpd=F,plt=c(1,1,1,1),col=fill.males,cex.lab=cex.lab,axisnames=F)
	par(new=T)
	barplot(females,width=widths,horiz=T,space=0,xlim=xlim,col=fill.females,ylim=ylim,axes=F,xlab="",
			border=border.females,ylab="",xpd=F,plt=c(1,1,1,1),axisnames=F)
	par(new=T)
	# axes 
	axis(2,at=yaxat,labels=yax.lab,las=1)
	axis(1,at=xaxat,labels=xax.lab,las=1)
	
	# option generations on right axis
	if (generations==T){
		gen <- seq(from=year,to=year-ages[length(ages)])  # the generations- this is why you state the year
		gen0 <- gen[gen\%\%10==0] # this is where the code figures out where the even numbered generations are (by 10)
		genat <- seq(from=max(gen)-max(gen0),to=max(gen)-min(gen),by=10) # where to plot the generations to track (in case of iterative plotting)
		axis(4,at=genat,labels=gen0,las=1)
		abline(h=genat,lty=gen.lty,col=gen.col,lwd=gen.lwd)
		mtext("generation",4,padj=4,cex=cex.lab)
	}
	}

}

\keyword{ hplot }
