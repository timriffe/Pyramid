#'
#' @title a function for plotting population pyramids
#' @description The Pyramid package provides a simple wrapper to \code{barplot()} with several optional arguments and defaults to quickly plot a population pyramid, and with automatic detection and plotting of multistate pyramids. The function also gives optional absolute or percent scales, with flexible age-group widths, optional cohort labels on the right axis, and optional reference lines.
#' 
#' @param males either a numeric vector of male population counts by age or a matrix or data.frame of the  male population, where each column is a state (e.g. employed, unemployed).
#' @param females either a numeric vector of female population counts by age or a matrix or data.frame  of the female population, where each column is a state (e.g. employed, unemployed).
#' @param widths numeric vector of the age-interval widths; must be the same length as \code{males} and \code{females}. If missing, defaults to \code{rep(1,length(males))}. Population counts are always divided by the interval widths for plotting. This makes bar magnitudes comparable between pyramids with different age intervals; bars are to be interpreted as single year ages, where all of the single ages within a 5-year class have the same value. Ages for axes are computed from \code{widths}.
#' @param prop logical. Should the x-axis be in percent or absolute value? Defaults to \code{TRUE}(percent axis). If absolute, the function tries to guess how many 0s to include in tick labels, and indicates  millions or thousands in the axis labels.
#' @param standardize logical. Default \code{TRUE}. Should bar volumes be made comparable with single-age pyramids (and hence with each other)? If \code{TRUE}, counts are divided by widths. Otherwise, counts are taken ``as-is''.
#' @param fill.males (\code{fill.females}) The fill color for the male bars (left side) and female bars (right side). Can be specified in any way that R accepts. Defaults are \code{"orange"} and \code{"purple"}, respectively, for simple pyramids, or \code{rainbow(k)} for multistate pyramids, where k is the number of states (columns in the input data).
#' @param border.males (\code{border.females}) border color for the male and female bars. Can be specified in any way that R accepts. Default = \code{"transparent"}.   
#' @param grid logical. Defaults to \code{TRUE}. Should reference lines be drawn across the plot for age, cohort or x axis ticks? 
#' @param grid.lty (\code{grid.col, grid.lwd, grid.bg}) graphical parameters for default reference lines. These get passed on to age, cohort and vertical reference lines unless specified specifically. \code{grid.lty} is analogous to \code{lty}, \code{grid.col} to \code{col} and so forth. Background color can be specified with \code{grid.bg}. Defaults are \code{2}, \code{"grey"}, \code{1} and \code{"transparent"}, respectively.
#' @param coh.axis logical. Defaults to \code{FALSE}. Should a cohort axis be drawn on the right side? In this case, you must also specify \code{year} to correctly compute birth cohorts. Ticks will be placed automatically on years ending in 0.
#' @param coh.lines logical. Defaults to \code{FALSE}. Should cohort reference lines be drawn across the plot? If switched to \code{TRUE}, the lines will match tick mark locations (on 0s) and copy the graphical parameters from \code{grid.}.
#' @param year The data year. This is only necessary if you want a cohort axis. Default value is 2000, so be careful!
#' @param coh.lty (\code{coh.col, coh.lwd}) graphical parameters for cohort reference lines. Defaults to the \code{grid.} values.
#' @param coh.lines.min logical. Defaults to \code{FALSE}. Should minor reference lines be drawnfor cohorts as well? 
#' @param coh.at.min optional vector of cohort values at which to draw minor reference lines. Defaults to cohort years evenly divisible by 5.
#' @param coh.lty.min (\code{coh.col.min, coh.lwd.min}) graphical parameters for minor cohort reference lines. Defaults are \code{3}, \code{coh.col} and \code{coh.lwd * .8}, respectively.
#' @param age.lines logical. Defaults to \code{TRUE}. Should age reference lines be drawn across the plot? Defaults to the value of the age axis ticks and copies the graphical parameters from \code{grid.}.
#' @param age.lty (\code{age.col, age.lwd}) optional graphical parameters to control the appearance of age grid reference lines. Defaults come from \code{grid.}
#' @param age.lines.min logical. Defaults to \code{FALSE}. Should minor reference lines be drawn for ages as well? 
#' @param age.at.min optional vector of age values at which to draw minor reference lines.  Defaults to ages evenly divisible by 5.
#' @param age.lty.min (\code{age.col.min, age.lwd.min}) optional graphical parameters to control the appearance of minor age reference minor lines. Defaults come from \code{age.}
#' @param v.lines logical. Defaults to \code{TRUE}. Should vertical reference lines be drawn across the plot? Defaults to the value of the x axis ticks and copies the graphical parameters from \code{grid.}.
#' @param v.lty (\code{v.col, v.lwd}) optional graphical parameters to control the appearance of vertical grid reference lines. Defaults come from \code{grid.}
#' @param v.lines.min logical. Defaults to \code{FALSE}. Should minor reference lines be drawn for vertical grid direction as well? 
#' @param v.at.min optional vector of x axis values at which to draw minor reference lines. Defaults to \code{pretty(xlim, n = 25, min.n = 16)}.
#' @param v.lty.min (\code{v.col.min, v.lwd.min}) optional graphical parameters to control the appearance of minor vertical reference lines. Defaults come from \code{v.}
#' @param main optional plot title, defaults to just telling you the total population. To not plot a title, simple specify \code{""}.
#' @param cex.main character expansion of plot title. Defaults to 1, normal plot title size.
#' @param xlim (\code{ylim}) x and y limits for the plot. Remember that even though axis labels for males on the left side are strictly positive, it is plotted in negative coordinate space
#' @param mar plot margins. Same as \code{par("mar"}. Defaults to \code{c(5,5,5,5)}.
#' @param xlab x axis label. Defaults to the most reasonable of \code{"percent"}, \code{"population (1000s)"} or \code{"population (millions)"}
#' @param ylab.left left (age) axis label. Defaults to \code{"Age"}
#' @param ylab.right right (cohort) axis label. Defaults to \code{"Cohort"}. Only plots if you specify \code{coh.axis = TRUE}.
#' @param cex.lab character expansion of axis labels. Defaults to 1, normal \code{xlab} and \code{ylab} sizes. Applies to \code{xlab}, \code{ylab.right} and \code{ylab.left}.
#' @param xax.at optional, values at which to draw x axis ticks. Default makes use of \code{pretty()}
#' @param xax.lab optional, labels to print on x axis ticks.
#' @param age.at optional, values at which to draw age (left) axis ticks. Defaults to multiples of 10.
#' @param age.lab optional, labels to print on age axis ticks.
#' @param coh.at optional, values at which to draw cohort (left) axis ticks. Defaults to multiples of 10
#' @param coh.labs optional, labels to print on cohort axis ticks.
#' @param cex.axis character expansion of all tick labels (x, age, and cohort).
#' @param box logical. Defaults to \code{TRUE}. Draw box around plot area.
#' @param verbose logical. Should informative but potentially annoying messages be returned when the function does something you might want to know about? Defaults to \code{TRUE}.
#' @details In most cases, this function has all the options that you might need for a quick 
#' population pyramid, should allow for a wide variety of styles and be flexible to different 
#' age groupings. See examples below for a demonstration of features. If you really want to 
#' have full control over the design, look at the function code for ideas about the \code{barplot()} 
#' settings that are needed to get started. 
#'
#' @author Tim Riffe \email{tim.riffe@@gmail.com}
#' 
#' @note suggestions welcome
#' 
#' @return \code{NULL} function called for its side effects.
#' 
#' @export
#' 
#' @seealso See also the \code{pyramid} package on CRAN, which plots the age axis in the middle.
#'
#' @examples
#'    data(PTpop)
#'    head(PTpop)
#'    
#'# default 
#'    Pyramid(males=PTpop[,1], females=PTpop[,2])
#'    
#'# remove messages
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            verbose = FALSE)
#'    
#'# add cohort axis on right:
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            verbose = FALSE,
#'            coh.axis = TRUE)
#'    
#'# but watch out! it needs to know the data year to get the cohorts right! (assumes year 2000)
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            verbose = FALSE,
#'            coh.axis = TRUE,
#'            year = 1950)
#'    
#'# you can change gridline parameters using grid.lty, grid.col, grid.lwd, grid.bg
#'# or you can turn them off using grid = FALSE
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            coh.axis=TRUE,
#'            verbose = FALSE,
#'            grid.lty=1,
#'            grid.lwd=.5)
#'    
#'# for instance, something close to the ggplot2 aesthetic
#'# give bars a border using border.males and border.females
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            verbose = FALSE,
#'            coh.axis=TRUE,
#'            grid.lty=1,
#'            grid.lwd=.5,
#'            grid.bg = gray(.9),
#'            grid.col = "white",
#'            border.males="black",
#'            border.females="black")
#'    
#'# get axis labels, title using arguments main, xlab, ylab.left and ylab.right 
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            coh.axis=TRUE,
#'            verbose = FALSE,
#'            grid.lty=1,
#'            grid.lwd=.5,
#'            grid.bg = gray(.9),
#'            grid.col = "white",
#'            border.males="black",
#'            border.females="black",
#'            xlab = "Porcentage",
#'            ylab.left = "Edad",
#'            ylab.right = "Cohorte",
#'            main = "Portugal Population Structure, 1950")
#'    
#'# change bar colors using fill.males and fill.females
#'# change x axis from percentage to absolute
#'# only vertical reference lines
#'# notice that the right ylab only shows if the axis is drawn:
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            verbose = FALSE,
#'            grid.lty=1,
#'            age.lines = FALSE,
#'            border.males="black",
#'            border.females="black",
#'            ylab.left = "Edad",
#'            ylab.right = "Cohorte",
#'            xlab = "poblaci\'{o}n (1000s)",
#'            main = "Portugal Population Structure, 1950",
#'            fill.males = "orange",
#'            fill.females = "yellow",
#'            prop = FALSE)
#'    
#'# get age on both sides by tricking the function using
#'# coh.at and coh.labs (coh.at wants cohort positions, coh.labs in ths case are age labels)
#'# you can alter age, vertical and cohort lines independently, using
#'# v.col, v.lty, v.lwd, etc, coh._, age._
#'# here we explicitly leave the right axis label blank, otherwise 'cohort' will show up
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            verbose = FALSE,
#'            grid.lty=1,
#'            v.lty = 2,
#'            coh.axis = TRUE,
#'            border.males="black",
#'            border.females="black",
#'            xlab = "Poblaci\'{o}n (1000s)",
#'            ylab.left = "Edad",
#'            ylab.right = "",
#'            main = "Portugal Population Structure, 1950",
#'            prop = FALSE,
#'            fill.males = "orange",
#'            fill.females = "yellow",
#'            coh.at = 1950 - seq(0,100,by = 10),
#'            coh.labs = seq(0,100, by = 10)
#'    )
#'    
#'# get vertical lines every .05, but only labeled every .2 (with prop = TRUE)
#'    
#'# coh., v. and age. graphical parameters can also refer to minor lines using the .min suffix,
#'# e.g. v.lty becomes v.lty.min
#'# we can turn on automatic minor lines using 
#'# v.lines.min = TRUE, age.lines.min = TRUE, coh.lines.min = TRUE
#'    
#'# also, you can give explicit character string labels for the x axis 
#'# (not for y axes, they must be numeric, sorry)- in this case some nice percents
#'# good idea to give explicit locations using xax.at
#'    
#'# made cex.axis smaller so labels would fit well
#'    xlabats <- seq(from = -1.2,to = 1.2, by = .2)
#'    xlabs <- c("1.2\%","1.0\%","0.8\%","0.6\%","0.4\%","0.2\%","0.0\%","0.2\%","0.4\%","0.6\%","0.8\%","1.0\%","1.2\%")
#'    
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            verbose = FALSE,
#'            grid.lty=1,
#'            v.lty = 2,
#'            coh.axis = TRUE,
#'            coh.lines = FALSE,
#'            border.males="black",
#'            border.females="black",
#'            xlab = "Porcentage",
#'            ylab.left = "Edad",
#'            ylab.right = "",
#'            main = "Portugal Population Structure, 1950",
#'            fill.males = "orange",
#'            fill.females = "yellow",
#'            coh.at = 1950 - seq(0,100,by = 10),
#'            coh.labs = seq(0,100, by = 10),
#'            xax.at = xlabats,
#'            xax.lab = xlabs,
#'            v.lines.min = TRUE,
#'            cex.axis=.75
#'    )
#'    
#'    
#'# specify some custom minor tick marks for age
#'# (change to taste!)
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            verbose = FALSE,
#'            coh.lines = FALSE,
#'            coh.axis = TRUE,
#'            year = 1950,
#'            border.males="black",
#'            border.females="black",
#'            xlab = "Porcentage",
#'            ylab.left = "Edad",
#'            ylab.right = "",
#'            main = "Portugal Population Structure, 1950",
#'            fill.males = "orange",
#'            fill.females = "yellow",
#'            coh.at = 1950 - seq(0,100,by = 10),
#'            coh.labs = seq(0,100, by = 10),
#'            xax.at = xlabats,
#'            xax.lab = c("1.2\%","1.0\%","0.8\%","0.6\%","0.4\%","0.2\%","0.0\%","0.2\%","0.4\%","0.6\%","0.8\%","1.0\%","1.2\%"),
#'            cex.axis=.75,
#'            age.lines.min = TRUE,
#'            age.at.min = seq(0,100,by = 2),
#'            age.col.min = gray(.7),
#'            age.lty.min = 3,
#'            age.lwd.min = .7
#'    )
#'    
#'    
#'# plot a transparent pyramid on top of another pyramid:
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            verbose = FALSE,
#'            grid.lty=1,
#'            coh.lines = FALSE,
#'            xlab = "Porcentage",
#'            ylab.left = "Edad",
#'            ylab.right = "",
#'            main = "Portugal Population Structure, 1950 & 2001",
#'            fill.males = "orange",
#'            fill.females = "yellow",
#'            border.males="black",
#'            border.females="black",
#'            coh.axis = TRUE,
#'            coh.at = 1950 - seq(0,100,by = 10),
#'            coh.labs = seq(0,100, by = 10),
#'            xax.at = xlabats,
#'            xax.lab = c("1.2\%","1.0\%","0.8\%","0.6\%","0.4\%","0.2\%","0.0\%","0.2\%","0.4\%","0.6\%","0.8\%","1.0\%","1.2\%"),
#'            cex.axis=.75
#'    )
#'# use par(new=TRUE) to allow plotting on top
#'# turn off several parameters, like grid, labels, make fill explicitly transparent (or NA)
#'    
#'# be carefully to explicitly define xlim and ylim for both plots so they match dimensions perfectly
#'    par(new=TRUE)
#'    Pyramid(males = PTpop[,3], 
#'            females = PTpop[,4],
#'            verbose = FALSE,
#'            grid=FALSE,
#'            border.males="black",
#'            border.females="black",
#'            xlab = "",
#'            ylab.left = "",
#'            ylab.right = "",
#'            main = "",
#'            fill.males = NA,
#'            fill.females = NA,
#'            xlim=c(-1.2,1.2),
#'            ylim=c(0,101),
#'            xax.at = xlabats,
#'            xax.lab = c("1.2\%","1.0\%","0.8\%","0.6\%","0.4\%","0.2\%","0.0\%","0.2\%","0.4\%","0.6\%","0.8\%","1.0\%","1.2\%"),
#'            cex.axis=.75,
#'            coh.axis = FALSE
#'    )
#'    
#'    ## or a semi-transparent pyramid where you can see the grid lines behind it:
#'    
#'# define transparency function for named colors:
#'    colalpha <- function(color,alpha){
#'        colalphai <- function(color,alpha){
#'           paste(rgb(t(col2rgb(color)/255)),alpha,sep="")
#'        }
#'        sapply(color,colalphai,alpha=alpha)
#'    }
#'    
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            verbose = FALSE,
#'            grid.lty = 1,
#'            grid.col = gray(.7),
#'            coh.lines = FALSE,
#'            xlab = "Porcentage",
#'            ylab.left = "Edad",
#'            ylab.right = "",
#'            main = "Portugal Population Structure, 1950",
#'            fill.males = colalpha("orange",90),
#'            fill.females = colalpha("yellow",90),
#'            border.males="black",
#'            border.females="black",
#'            coh.axis = TRUE,
#'            coh.at = 1950 - seq(0,100,by = 10),
#'            coh.labs = seq(0,100, by = 10),
#'            xax.at = xlabats,
#'            xax.lab = c("1.2\%","1.0\%","0.8\%","0.6\%","0.4\%","0.2\%","0.0\%","0.2\%","0.4\%","0.6\%","0.8\%","1.0\%","1.2\%"),
#'            cex.axis = .75,
#'            # minor grid line arguments:
#'            v.lines.min = TRUE,
#'            v.lty.min = 1,
#'            v.lwd.min = .7,
#'            v.col = "black"
#'    )
#'    
#'    
#'# another option would be to simply draw the reference lines on top of the pyramid:
#'    
#'# specify grid = FALSE
#'# then draw manually 
#'    Pyramid(males = PTpop[,1], 
#'            females = PTpop[,2],
#'            year = 1950,
#'            verbose = FALSE,
#'            grid = FALSE,
#'            xlab = "Porcentage",
#'            ylab.left = "Edad",
#'            ylab.right = "",
#'            main = "Portugal Population Structure, 1950",
#'            fill.males = "orange",
#'            fill.females = "yellow",
#'            coh.axis = TRUE,
#'            coh.at = 1950 - seq(0,100,by = 10),
#'            coh.labs = seq(0,100, by = 10),
#'            xax.at = xlabats,
#'            xax.lab = c("1.2\%","1.0\%","0.8\%","0.6\%","0.4\%","0.2\%","0.0\%","0.2\%","0.4\%","0.6\%","0.8\%","1.0\%","1.2\%"),
#'            cex.axis=.75,
#'    )
#'# plot minor reference lines
#'    segments(-1.2,seq(2,100,by=2),1.2,seq(2,100,by=2), col = colalpha(gray(.2),50), lwd = .8, lty = 3)
#'    segments(seq(-1.2,1.2,by=.1),0,seq(-1.2,1.2,by=.1), 101, col = colalpha(gray(.2),50), lwd = .8, lty = 3)
#'# plot major reference lines
#'    segments(-1.2,0:10 * 10,1.2, 0:10 *10, col = colalpha(gray(.4),50),lwd=.8)
#'    segments(seq(-1.2,1.2,by=.2),0,seq(-1.2,1.2,by=.2), 101, col = colalpha(gray(.4),50),lwd=.8)
#'    
#'# example of a multistate pyramid.
#'    data(ESextr)
#'    males <- matrix(as.numeric(ESextr[1:18,2:5]),ncol=4)
#'    females <- matrix(as.numeric(ESextr[19:36,2:5]),ncol=4)
#'    widths <- rep(5,nrow(ESextr)/2)
#'    
#'    Pyramid(males,
#'            females,
#'            widths=widths,
#'            main = "Foreign population in Spain  (registered) by continent of bith, 2010",
#'            fill.males=gray(c(.8,.6,.4,.2)),
#'            fill.females=gray(c(.8,.6,.4,.2))
#'    )
#'    rect(1,70,2,90,col="white",border="black")
#'    legend(1,90,legend=c("Europe","Africa","Americas","Asia"),fill=gray(c(.8,.6,.4,.2)),xpd=TRUE,bty="n")
#'    
#'# goes well together with individual subgroup plots.
#'# remember, specify equal x axes for all of them:
#'# african males force us to bring the axes out to 3!
#'    countries <- c("Europe","Africa","Americas","Asia")
#'    dev.new(height=8,width=8)
#'    par(mfrow=c(2,2))
#'    for (i in 1:4){
#'        Pyramid(males[,i],
#'                females[,i],
#'                widths=widths,
#'                main = countries[i],
#'                fill.males=gray(.4),
#'                fill.females=gray(.6),
#'                xlim=c(-3,3),
#'                mar=c(2,2,2,2)
#'        )
#'    }
#'    
#'
#' @keywords hplot 
#' 

Pyramid <-
		function(males,	females, widths, prop = TRUE, standardize = TRUE,
				fill.males,	fill.females, border.males="transparent", border.females="transparent",
				grid = TRUE, grid.lty = 2, grid.col = "grey",	grid.lwd = 1, grid.bg = "transparent",
				coh.axis = FALSE, coh.lines = FALSE, year = 2000, coh.lty, coh.col, coh.lwd,
				coh.lines.min = FALSE, coh.at.min, coh.lty.min, coh.col.min, coh.lwd.min,
				age.lines = TRUE, age.lty, age.col, age.lwd, 
				age.lines.min = FALSE, age.at.min, age.lty.min, age.col.min, age.lwd.min,
				v.lines = TRUE, v.lty, v.col, v.lwd, 
				v.lines.min = FALSE, v.at.min, v.lty.min, v.col.min, v.lwd.min,
				main, xlim, ylim, cex.main = 1, cex.lab = 1, mar,
				xlab, ylab.left = "Age", ylab.right = "Cohort", 
				xax.at, xax.lab, age.at, age.lab, coh.at, coh.labs, cex.axis = 1,
				box = TRUE, verbose = TRUE){
    # %TODO: add outline option, middle axis, confidence intervals?
	# short verbose code
	Verb <- function(v, x){
		if (v) {
			cat(paste(x, "\n", sep = ""))
		}
	}
	
	# check that male and female data are of equal length 
	if (length(males) != length(females)) {stop("the vectors input for males and females are not of the same length\nthis function is too dumb to know what you want. sorry")}
	
	# for simple pyramids, k = 1; for multistate pyramids, we need a separate column for each of k states
	if (is.null(dim(males))){
		k <- 1
	} else { k <- ncol(males) }
	
	# we just assume 1-year age groups unless widths are specified, the user ought to immediately notice if something is wrong here...
	if (missing(widths)) {
		widths <- rep(1, (length(males)/k))
		Verb(verbose, "widths unspecified; assumed 1-year age intervals; this affects the pyramid dimensions and ages")
	}
	
	# in either case, we deduce ages from the age interval widths.
	ages <- cumsum(widths) - widths
	
	# total population 
	tot <- sum(males) + sum(females) 
	
    # this toggles whether we standardize or not
    if (standardize){
        denom.widths <- widths
    } else {
        denom.widths <- 1
    }
	# for figuring out axes
	xmax1 <- if (is.null(dim(males))) {
                max(c(males, females) / denom.widths)
            } else {
                max(c(rowSums(males), rowSums(females) / denom.widths))
            }
	
	# should we round to 1000s or 1000000s for axis labels?
	KorM 							<- 10 ^ (3 * floor(log10(xmax1)/3))
	KorMTitle 						<- (10 ^ (3 * floor(log10(tot)/3)))
	KorMlab 						<- ifelse(KorMTitle == 1000, "(1000s)", "(millions)")
	
	# deciding proper x axes and labels in accordance with prop:
	if (prop){
		males 						<- (males / tot * -100) / denom.widths # -% males (so that they plot to the left)
		females 					<- (females / tot * 100) / denom.widths # % females, plotting to the right
		if (k == 1) {
			xmax 					<- max(c(-males, females))
		} else {
			xmax 					<- max(c(rowSums(-males), rowSums(females)))
			males 					<- t(males)
			females 				<- t(females)
		}
		if (missing(xlab)) {xlab 	<- "percent"}
	} else{
		males 						<- (-males / KorM) / denom.widths
		females 					<- (females / KorM) / denom.widths
		if (k == 1) {
			xmax 					<- max(c(-males, females))
		} else {
			xmax 					<- max(c(rowSums(-males), rowSums(females)))
			males 					<- t(males)
			females 				<- t(females)
		}
		if (missing(xlab)) {xlab 	<- paste("population"," (", KorM, "s)",sep="")}
	}
	
	# scipen=6 makes sure the labels aren't in scientific notation
	options(scipen = 6)
	
	# if both xlim and xax.at left blank: 
	if (all(missing(xax.at), missing(xlim))) {
		xax.at 						<- pretty(c(-xmax, xmax), 8)
		xlim 						<- range(xax.at)
	}
	
	# cohorts: (reason for stating year)
	gen	 		<- seq(from = year, to = year - ages[length(ages)])  
	# this allows users to specify cohorts on a cohort scale, we then shift back to age scale
	# if coh.at is specified but not coh.labs, we take coh.labs from coh.at, then shift coh.at to age scale
	
	if (!missing(coh.at))	{
		if (missing(coh.labs)) 			{coh.labs 			<- coh.at}
		coh.at				<- ages[gen %in% coh.at]
	}
	# default plot cohorts evenly divisible by 10
	if (missing(coh.labs)) 				{coh.labs 			<- gen[gen %% 10 == 0]}
	if (missing(coh.at))				{coh.at 			<- ages[gen %in% coh.labs]} 
	# where to plot the generations to track (in case of iterative plotting)
	
	# plotting limits
	if (missing(ylim)) 					{ylim 				<- range(ages) + c(0, widths[length(widths)])}
	if (missing(xlim)) 					{xlim 				<- range(xax.at)}
	
	# if xlim given, but xax.at not given
	if (missing(xax.at)) 				{xax.at 			<- pretty(xlim, n = 10, min.n = 8)}
	
	# labels for x ticks
	if (missing(xax.lab)) 				{xax.lab 			<- abs(xax.at)}
	
	# age labels
	if (missing(age.at)) 				{age.at 			<- ages[ages %% 10 == 0]}
	if (missing(age.lab)) 				{age.lab 			<- age.at}
	
	# some default colors
	# k > 1 identifies multistate pyramids, for which colors go left to right along columns in the input data, in to out in the pyramid itself
	if (k > 1){
		if (missing(fill.males)) 		{fill.males 		<- rainbow(k)}
		if (missing(fill.females)) 		{fill.females 		<- rainbow(k)}
		if (missing(border.males)) 		{border.males 		<- "black"}
		if (missing(border.females)) 	        {border.females 	<- "black"}
	}
	if (k == 1){
		if (missing(fill.males)) 		{fill.males 		<- "orange"}
		if (missing(fill.females)) 		{fill.females 		<- "purple"}
		if (missing(border.males)) 		{border.males 		<- "transparent"}
		if (missing(border.females))    {border.females 	<- "transparent"}
	}
	
	# a default title, if year is not specified then 2000 is used, and the user ought to notice. This is so that
	# the user doesn't accidentally allow the generations to be calculated using the year 2000.
	if (missing(main)){ main <- paste("Total Pop = ",round(tot/KorMTitle,digits=1),KorMlab)}
	
	# default margins
	if (missing(mar))				{mar 			<- c(5,5,5,5) + 0.1}
	par(mar = mar)
	
	# -----------------------------------------------
	# optional grid/ reference lines
	if (grid){
		# note: an empty plot didn't work because the axes scaling is different..
		barplot(0,0, horiz=TRUE, axes = FALSE, xlim = xlim, ylim = ylim, xlab = "", ylab = "", plt = c(1, 1, 1, 1))
		rect(xlim[1], ylim[1], xlim[2], ylim[2], col = grid.bg, border = "transparent")
		# vertical lines on ticks
		if (v.lines){
			if (missing(v.col))			{v.col 				<- grid.col}
			if (missing(v.lty))			{v.lty 				<- grid.lty}
			if (missing(v.lwd))			{v.lwd 				<- grid.lwd}
			segments(xax.at, ylim[1], xax.at, ylim[2], col = v.col, lty = v.lty, lwd = v.lwd)	
			#optional minor vertical reference lines
			if (v.lines.min){
				if (missing(v.at.min))	{v.at.min 			<- pretty(xlim, n = 25, min.n = 16)}
				if (missing(v.col.min))	{v.col.min 			<- v.col}
				if (missing(v.lty.min))	{v.lty.min 			<- 3}
				if (missing(v.lwd.min))	{v.lwd.min 			<- v.lwd*.8}
				segments(v.at.min, ylim[1], v.at.min, ylim[2], col = v.col.min, lty = v.lty.min, lwd = v.lwd.min)	
			}
		}
		# age lines
		if (age.lines){
			if (missing(age.col))		{age.col 			<- grid.col}
			if (missing(age.lty))		{age.lty 			<- grid.lty}
			if (missing(age.lwd))		{age.lwd 			<- grid.lwd}
			segments(xlim[1], age.at, xlim[2], age.at, lty = age.lty, col = age.col, lwd = age.lwd)
			# optional minor reference lines for ages
			if (age.lines.min){
				if (missing(age.at.min))	{age.at.min 			<- ages[ages %% 5 == 0]}
				if (missing(age.col.min))	{age.col.min 			<- age.col}
				if (missing(age.lty.min))	{age.lty.min 			<- 3}
				if (missing(age.lwd.min))	{age.lwd.min 			<- age.lwd*.8}
				segments(xlim[1],age.at.min,xlim[2],age.at.min, col = age.col.min, lty = age.lty.min, lwd = age.lwd.min)	
			}
		}
		# cohort lines
		if (coh.lines){
			if (missing(coh.col))		{coh.col 			<- grid.col}
			if (missing(coh.lty))		{coh.lty 			<- grid.lty}
			if (missing(coh.lwd))		{coh.lwd 			<- grid.lwd}
			segments(xlim[1], coh.at, xlim[2], coh.at, lty = coh.lty, col = coh.col, lwd = coh.lwd)
			# optional minor reference lines for cohorts
			if (coh.lines.min){
				if (missing(coh.at.min))	{coh.at.min 			<- seq(from = max(gen) - gen[gen %% 5 == 0], to = max(gen) - min(gen), by = 5)}
				if (missing(coh.col.min))	{coh.col.min 			<- coh.col}
				if (missing(coh.lty.min))	{coh.lty.min 			<- 3}
				if (missing(coh.lwd.min))	{coh.lwd.min 			<- coh.lwd*.8}
				segments(xlim[1],coh.at.min,xlim[2],coh.at.min, col = coh.col.min, lty = coh.lty.min, lwd = coh.lwd.min)	
			}
		}
		par(new=TRUE)
	}
	# -----------------------------------------------
	# the pyramid bars: left and right plotted separately
	barplot(males, width = widths, horiz = TRUE, space = 0, xlim = xlim, ylim = ylim, ylab = ylab.left, xlab = xlab, border = border.males,
			main = main, axes = FALSE, cex.main = cex.main, xpd = FALSE, plt = c(1, 1, 1, 1), col = fill.males, cex.lab = cex.lab, axisnames = FALSE)
	par(new = TRUE)
	barplot(females, width = widths, horiz = TRUE, space = 0, xlim = xlim, col = fill.females, ylim = ylim, axes = FALSE, xlab = "",
			border = border.females, ylab = "", xpd = FALSE, plt=c(1,1,1,1), axisnames = FALSE)
	
	# axes ------------------------------------------
	# age axis (left side)
	axis(2, at = age.at, labels = age.lab, las = 1, cex.axis = cex.axis, pos = xlim[1])
	# x axis
	axis(1, at = xax.at, labels = xax.lab, las = 1, cex.axis = cex.axis, pos = ylim[1])
	# cohort axis (right side)
	if (coh.axis){
		axis(4, at = coh.at, labels = coh.labs, las = 1, cex.axis = cex.axis, pos = xlim[2])
		mtext(side = 4, ylab.right, cex = cex.lab, line = 3)
	}
	# final bounding box
	if (box){
		rect(xlim[1], ylim[1], xlim[2], ylim[2], col = "transparent")
	}
}

# some data documentation
#'
#'Spanish foreign population by continent of origin, 2010.
#'
#'Four continents, 5-year age groups, males and females.
#'
#'
#'@name ESextr
#'@docType data
#'@format The format is: chr [1:36, 1:5] "males" "males" "males" "males"
#'"males" ...  - attr(*, "dimnames")=List of 2 ..$ : chr [1:36] "0-4" "5-9"
#'"10-14" "15-19" ...  ..$ : chr [1:5] "" "PAISES.EUROPEOS" "PAISES.AFRICANOS"
#'"PAISES.AMERICANOS" ...
#'@source Instituto Nacional de Estadistica (INE). Cifras oficiales de
#'población: Revisión del Padrón municipal. Población a 1 de enero de 2010 (23
#'diciembre 2010). Available at: www.ine.es
#'@keywords datasets
#' @export
#'@examples
#'
#'data(ESextr)
#'
#'head(ESextr)
#'is.matrix(ESextr) # TRUE
#'is.character(ESextr) # TRUE
#'# sorry for the slop: need numeric data: 
#'males <- matrix(as.numeric(ESextr[1:18,2:5]),ncol=4)
#'females <- matrix(as.numeric(ESextr[19:36,2:5]),ncol=4)
#'
#'widths <- rep(5,nrow(ESextr/2))
#'
#'# composition of total registered foreign population by continent of origin:
#'Pyramid(males,
#'      females,
#'      widths=widths,
#'      main = "Foreign population in Spain  (registered) by continent of bith, 2010",
#'      fill.males=gray(c(.8,.6,.4,.2)),
#'      fill.females=gray(c(.8,.6,.4,.2))
#')
#'rect(1,70,2,90,col="white",border="black")
#'legend(1,90,legend=c("Europe","Africa","Americas","Asia"),fill=gray(c(.8,.6,.4,.2)),xpd=TRUE,bty="n")
#'
#'# or separately, easier comparison of structure by age and sex. Fix x limits!
#'countries <- c("Europe","Africa","Americas","Asia")
#'dev.new(height=8,width=8)
#'par(mfrow=c(2,2))
#'for (i in 1:4){
#'  Pyramid(males[,i],
#'          females[,i],
#'          widths=widths,
#'          main = countries[i],
#'          fill.males=gray(.4),
#'          fill.females=gray(.6),
#'          xlim=c(-3,3),
#'          mar=c(2,2,2,2)
#'  )
#'}
#'
NULL

#'Portugal population counts by age and sex, years 1950 and 2001.
#'
#'Portugal population counts by age and sex, years 1950 and 2001. Single ages.
#'
#'ages 0 to 100.
#'
#'@name PTpop
#'@docType data
#'@format A data frame with 101 observations on the following 4 variables.
#'\describe{ \item{list("males_1950")}{a numeric vector}
#'\item{list("females_1950")}{a numeric vector} \item{list("males_2001")}{a
#'numeric vector} \item{list("females_2001")}{a numeric vector} }
#'@references Available in: Dinâmicas Demográficas e Envelhecimento da
#'População Portuguesa: Evolução e Perspectivas. Instituto do Envelhecimento,
#'Universidade de Lisboa.
#'@source Data aggregated from Portuguese census microdata from: Instituto
#'Nacional de Estatística (INE). http://www.ine.pt.
#'@keywords datasets
#' @export
#'@examples
#'
#'data(PTpop)
#'head(PTpop)
#'
#'Pyramid(PTpop[,1],PTpop[,2])
#'
#'# check for digit preference / age heaping. Higher = more heaping.
#'apply(PTpop,2,MyersI)
#'
#'
NULL

# package documentation
#'Function and example data for plotting population pyramids.
#'
#'The Pyramid package provides a simple wrapper to \code{barplot()} with
#'several defaults to quickly plot a population pyramid, and with simple
#'detection and plotting of multistate pyramids. The function also gives
#'optional absolute or percent scales, with flexible age-group widths, an
#'optional cohort axis, and major and minor reference lines.
#'
#'\tabular{ll}{ Package: \tab Pyramid\cr Type: \tab Package\cr Version: \tab
#'1.2\cr Date: \tab 2011-12-07\cr License: \tab GPL-2\cr }
#'
#'@name Pyramid-package
#'@aliases Pyramid-package Pyramid
#'@docType package
#'@author Tim Riffe\cr
#'
#'Maintainer: Tim Riffe <tim.riffe@@gmail.com>
#'@seealso \code{\link[pyramid]{pyramid}} ~~
#'@references Data from:\cr Instituto Nacional de Estadistica (INE). Cifras
#'oficiales de población: Revisión del Padrón municipal. Población a 1 de enero
#'de 2010 (23 diciembre 2010). Available at: www.ine.es\cr Instituto Nacional
#'de Estatística (INE). Data aggregated from Portuguese census microdata from:
#'http://www.ine.pt. \cr Available in: Dinâmicas Demográficas e Envelhecimento
#'da População Portuguesa: Evolução e Perspectivas. Instituto do
#'Envelhecimento, Universidade de Lisboa.\cr MyersI function based on:\cr
#'Myers, R.J. (1940) Errors and bias in the reporting of ages in census data.
#'Transactions of the Actuarial Society of America. vol 41 nr 2 pages
#'395--415\cr Shryock, H.S. and Siegel, J.S. and Larmon, E.A. (1980) The
#'methods and materials of demography. US Dept. of Commerce, Bureau of the
#'Census (pages 116-118)
#'@keywords package hplot
#' @export
#'@examples
#'
#'# same examples as given with Pyramid() plot function.
#'
#'data(PTpop)
#'head(PTpop)
#'
#'# default 
#'Pyramid(males=PTpop[,1], females=PTpop[,2])
#'
#'# remove messages
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      verbose = FALSE)
#'
#'# add cohort axis on right:
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      verbose = FALSE,
#'      coh.axis = TRUE)
#'
#'# but watch out! it needs to know the data year to get the cohorts right! (assumes year 2000)
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      verbose = FALSE,
#'      coh.axis = TRUE,
#'      year = 1950)
#'
#'# you can change gridline parameters using grid.lty, grid.col, grid.lwd, grid.bg
#'# or you can turn them off using grid = FALSE
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      coh.axis=TRUE,
#'      verbose = FALSE,
#'      grid.lty=1,
#'      grid.lwd=.5)
#'
#'# for instance, something close to the ggplot2 aesthetic
#'# give bars a border using border.males and border.females
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      verbose = FALSE,
#'      coh.axis=TRUE,
#'      grid.lty=1,
#'      grid.lwd=.5,
#'      grid.bg = gray(.9),
#'      grid.col = "white",
#'      border.males="black",
#'      border.females="black")
#'
#'# get axis labels, title using arguments main, xlab, ylab.left and ylab.right 
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      coh.axis=TRUE,
#'      verbose = FALSE,
#'      grid.lty=1,
#'      grid.lwd=.5,
#'      grid.bg = gray(.9),
#'      grid.col = "white",
#'      border.males="black",
#'      border.females="black",
#'      xlab = "Porcentage",
#'      ylab.left = "Edad",
#'      ylab.right = "Cohorte",
#'      main = "Portugal Population Structure, 1950")
#'
#'# change bar colors using fill.males and fill.females
#'# change x axis from percentage to absolute
#'# only vertical reference lines
#'# notice that the right ylab only shows if the axis is drawn:
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      verbose = FALSE,
#'      grid.lty=1,
#'      age.lines = FALSE,
#'      border.males="black",
#'      border.females="black",
#'      ylab.left = "Edad",
#'      ylab.right = "Cohorte",
#'      xlab = "poblaci\'{o}n (1000s)",
#'      main = "Portugal Population Structure, 1950",
#'      fill.males = "orange",
#'      fill.females = "yellow",
#'      prop = FALSE)
#'      
#'# get age on both sides by tricking the function using
#'# coh.at and coh.labs (coh.at wants cohort positions, coh.labs in ths case are age labels)
#'# you can alter age, vertical and cohort lines independently, using
#'# v.col, v.lty, v.lwd, etc, coh._, age._
#'# here we explicitly leave the right axis label blank, otherwise 'cohort' will show up
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      verbose = FALSE,
#'      grid.lty=1,
#'      v.lty = 2,
#'      coh.axis = TRUE,
#'      border.males="black",
#'      border.females="black",
#'      xlab = "Poblaci\'{o}n (1000s)",
#'      ylab.left = "Edad",
#'      ylab.right = "",
#'      main = "Portugal Population Structure, 1950",
#'      prop = FALSE,
#'      fill.males = "orange",
#'      fill.females = "yellow",
#'      coh.at = 1950 - seq(0,100,by = 10),
#'      coh.labs = seq(0,100, by = 10)
#')
#'      
#'# get vertical lines every .05, but only labeled every .2 (with prop = TRUE)
#'
#'# coh., v. and age. graphical parameters can also refer to minor lines using the .min suffix,
#'# e.g. v.lty becomes v.lty.min
#'# we can turn on automatic minor lines using 
#'# v.lines.min = TRUE, age.lines.min = TRUE, coh.lines.min = TRUE
#'
#'# also, you can give explicit character string labels for the x axis 
#'# (not for y axes, they must be numeric, sorry)- in this case some nice percents
#'# good idea to give explicit locations using xax.at
#'
#'# made cex.axis smaller so labels would fit well
#'xlabats <- seq(from = -1.2,to = 1.2, by = .2)
#'xlabs <- c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%")
#'
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      verbose = FALSE,
#'      grid.lty=1,
#'      v.lty = 2,
#'      coh.axis = TRUE,
#'      coh.lines = FALSE,
#'      border.males="black",
#'      border.females="black",
#'      xlab = "Porcentage",
#'      ylab.left = "Edad",
#'      ylab.right = "",
#'      main = "Portugal Population Structure, 1950",
#'      fill.males = "orange",
#'      fill.females = "yellow",
#'      coh.at = 1950 - seq(0,100,by = 10),
#'      coh.labs = seq(0,100, by = 10),
#'      xax.at = xlabats,
#'      xax.lab = xlabs,
#'      v.lines.min = TRUE,
#'      cex.axis=.75
#')
#'
#'
#'# specify some custom minor tick marks for age
#'# (change to taste!)
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      verbose = FALSE,
#'      coh.lines = FALSE,
#'      coh.axis = TRUE,
#'      year = 1950,
#'      border.males="black",
#'      border.females="black",
#'      xlab = "Porcentage",
#'      ylab.left = "Edad",
#'      ylab.right = "",
#'      main = "Portugal Population Structure, 1950",
#'      fill.males = "orange",
#'      fill.females = "yellow",
#'      coh.at = 1950 - seq(0,100,by = 10),
#'      coh.labs = seq(0,100, by = 10),
#'      xax.at = xlabats,
#'      xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
#'      cex.axis=.75,
#'      age.lines.min = TRUE,
#'      age.at.min = seq(0,100,by = 2),
#'      age.col.min = gray(.7),
#'      age.lty.min = 3,
#'      age.lwd.min = .7
#')
#'
#'
#'# plot a transparent pyramid on top of another pyramid:
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      verbose = FALSE,
#'      grid.lty=1,
#'      coh.lines = FALSE,
#'      xlab = "Porcentage",
#'      ylab.left = "Edad",
#'      ylab.right = "",
#'      main = "Portugal Population Structure, 1950 & 2001",
#'      fill.males = "orange",
#'      fill.females = "yellow",
#'      border.males="black",
#'      border.females="black",
#'      coh.axis = TRUE,
#'      coh.at = 1950 - seq(0,100,by = 10),
#'      coh.labs = seq(0,100, by = 10),
#'      xax.at = xlabats,
#'      xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
#'      cex.axis=.75
#')
#'# use par(new=TRUE) to allow plotting on top
#'# turn off several parameters, like grid, labels, make fill explicitly transparent (or NA)
#'
#'# be carefully to explicitly define xlim and ylim for both plots so they match dimensions perfectly
#'par(new=TRUE)
#'Pyramid(males = PTpop[,3], 
#'      females = PTpop[,4],
#'      verbose = FALSE,
#'      grid=FALSE,
#'      border.males="black",
#'      border.females="black",
#'      xlab = "",
#'      ylab.left = "",
#'      ylab.right = "",
#'      main = "",
#'      fill.males = NA,
#'      fill.females = NA,
#'      xlim=c(-1.2,1.2),
#'      ylim=c(0,101),
#'      xax.at = xlabats,
#'      xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
#'      cex.axis=.75,
#'      coh.axis = FALSE
#')
#'
#'## or a semi-transparent pyramid where you can see the grid lines behind it:
#'
#'# define transparency function for named colors:
#'colalpha <- function(color,alpha){
#'  colalphai <- function(color,alpha){
#'      paste(rgb(t(col2rgb(color)/255)),alpha,sep="")
#'  }
#'  sapply(color,colalphai,alpha=alpha)
#'}
#'
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      verbose = FALSE,
#'      grid.lty = 1,
#'      grid.col = gray(.7),
#'      coh.lines = FALSE,
#'      xlab = "Porcentage",
#'      ylab.left = "Edad",
#'      ylab.right = "",
#'      main = "Portugal Population Structure, 1950",
#'      fill.males = colalpha("orange",90),
#'      fill.females = colalpha("yellow",90),
#'      border.males="black",
#'      border.females="black",
#'      coh.axis = TRUE,
#'      coh.at = 1950 - seq(0,100,by = 10),
#'      coh.labs = seq(0,100, by = 10),
#'      xax.at = xlabats,
#'      xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
#'      cex.axis = .75,
#'      # minor grid line arguments:
#'      v.lines.min = TRUE,
#'      v.lty.min = 1,
#'      v.lwd.min = .7,
#'      v.col = "black"
#')
#'
#'
#'# another option would be to simply draw the reference lines on top of the pyramid:
#'
#'# specify grid = FALSE
#'# then draw manually 
#'Pyramid(males = PTpop[,1], 
#'      females = PTpop[,2],
#'      year = 1950,
#'      verbose = FALSE,
#'      grid = FALSE,
#'      xlab = "Porcentage",
#'      ylab.left = "Edad",
#'      ylab.right = "",
#'      main = "Portugal Population Structure, 1950",
#'      fill.males = "orange",
#'      fill.females = "yellow",
#'      coh.axis = TRUE,
#'      coh.at = 1950 - seq(0,100,by = 10),
#'      coh.labs = seq(0,100, by = 10),
#'      xax.at = xlabats,
#'      xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
#'      cex.axis=.75,
#')
#'# plot minor reference lines
#'segments(-1.2,seq(2,100,by=2),1.2,seq(2,100,by=2), col = colalpha(gray(.2),50), lwd = .8, lty = 3)
#'segments(seq(-1.2,1.2,by=.1),0,seq(-1.2,1.2,by=.1), 101, col = colalpha(gray(.2),50), lwd = .8, lty = 3)
#'# plot major reference lines
#'segments(-1.2,0:10 * 10,1.2, 0:10 *10, col = colalpha(gray(.4),50),lwd=.8)
#'segments(seq(-1.2,1.2,by=.2),0,seq(-1.2,1.2,by=.2), 101, col = colalpha(gray(.4),50),lwd=.8)
#'
#'# example of a multistate pyramid.
#'data(ESextr)
#'males <- matrix(as.numeric(ESextr[1:18,2:5]),ncol=4)
#'females <- matrix(as.numeric(ESextr[19:36,2:5]),ncol=4)
#'widths <- rep(5,nrow(ESextr)/2)
#'
#'Pyramid(males,
#'      females,
#'      widths=widths,
#'      main = "Foreign population in Spain  (registered) by continent of bith, 2010",
#'      fill.males=gray(c(.8,.6,.4,.2)),
#'      fill.females=gray(c(.8,.6,.4,.2))
#')
#'rect(1,70,2,90,col="white",border="black")
#'legend(1,90,legend=c("Europe","Africa","Americas","Asia"),fill=gray(c(.8,.6,.4,.2)),xpd=TRUE,bty="n")
#'
#'# goes well together with individual subgroup plots.
#'# remember, specify equal x axes for all of them:
#'# african males force us to bring the axes out to 3!
#'countries <- c("Europe","Africa","Americas","Asia")
#'dev.new(height=8,width=8)
#'par(mfrow=c(2,2))
#'for (i in 1:4){
#'  Pyramid(males[,i],
#'          females[,i],
#'          widths=widths,
#'          main = countries[i],
#'          fill.males=gray(.4),
#'          fill.females=gray(.6),
#'          xlim=c(-3,3),
#'          mar=c(2,2,2,2)
#'  )
#'}
#'
#'
NULL

