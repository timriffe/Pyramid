#setwd("/home/triffe/CODE/HELP/Alda")
setwd("/home/triffe/CODE/HELP/Alda")

# Age axis in the middle
#################### demonstration of Pyramid() arguments


setwd("C:\\Users\\triffe\\git\\Pyramid\\Pyramid")

PTpop <- read.table ("test\\PTpop.txt", header=TRUE, sep="\t")
rownames(PTpop) <- 0:100
years <- c(1950,1960,1970,1981,1991,2001)
colnames(PTpop) <- paste(rep(c("males","females"),6),rep(years,each=2),sep="_")
PTpop <- PTpop[,c(1,2,11,12)]



source("dev\\Pyramid.R")

# default 
Pyramid(males=PTpop[,1], females=PTpop[,2])

# remove warnings
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
		main = "Población de Portugal, 1950")

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
		xlab = "población (1000s)",
		main = "Población de Portugal, 1950",
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
		xlab = "Población (1000s)",
		ylab.left = "Edad",
		ylab.right = "",
		main = "Población de Portugal, 1950",
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
xlabs <- c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%")

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
		main = "Población de Portugal, 1950",
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
		main = "Población de Portugal, 1950",
		fill.males = "orange",
		fill.females = "yellow",
		coh.at = 1950 - seq(0,100,by = 10),
		coh.labs = seq(0,100, by = 10),
		xax.at = xlabats,
		xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
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
		main = "Población de Portugal, 1950 & 2001",
		fill.males = "orange",
		fill.females = "yellow",
		border.males="black",
		border.females="black",
		coh.axis = TRUE,
		coh.at = 1950 - seq(0,100,by = 10),
		coh.labs = seq(0,100, by = 10),
		xax.at = xlabats,
		xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
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
		xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
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
		main = "Población de Portugal, 1950",
		fill.males = colalpha("orange",90),
		fill.females = colalpha("yellow",90),
		border.males="black",
		border.females="black",
		coh.axis = TRUE,
		coh.at = 1950 - seq(0,100,by = 10),
		coh.labs = seq(0,100, by = 10),
		xax.at = xlabats,
		xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
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
		main = "Población de Portugal, 1950",
		fill.males = "orange",
		fill.females = "yellow",
		coh.axis = TRUE,
		coh.at = 1950 - seq(0,100,by = 10),
		coh.labs = seq(0,100, by = 10),
		xax.at = xlabats,
		xax.lab = c("1.2%","1.0%","0.8%","0.6%","0.4%","0.2%","0.0%","0.2%","0.4%","0.6%","0.8%","1.0%","1.2%"),
		cex.axis=.75,
)
# plot minor reference lines
segments(-1.2,seq(2,100,by=2),1.2,seq(2,100,by=2), col = colalpha(gray(.2),50), lwd = .8, lty = 3)
segments(seq(-1.2,1.2,by=.1),0,seq(-1.2,1.2,by=.1), 101, col = colalpha(gray(.2),50), lwd = .8, lty = 3)
# plot major reference lines
segments(-1.2,0:10 * 10,1.2, 0:10 *10, col = colalpha(gray(.4),50),lwd=.8)
segments(seq(-1.2,1.2,by=.2),0,seq(-1.2,1.2,by=.2), 101, col = colalpha(gray(.4),50),lwd=.8)

# -----------------------------------------------------------------------------------------#
# example of a multistate pyramid.
# in this case, not the greatest example, since you can't see the sex-imbalances for
# smaller groups, still good idea of overall composition

# this would be accompanied well by subgroup-specific pyramids, each scaled to 100.
males <- as.matrix(read.table("test/ESextr2010males.txt",header=TRUE,sep="\t"))
females <- as.matrix(read.table("test/ESextr2010females.txt",header=TRUE,sep="\t"))

rownames(males) <- rownames(females) <- paste(seq(0,85,by=5),c(seq(4,84,by=5),"+"),sep="-")
males <- cbind("males",males)
rownames(males)[18] <- "85+"
rownames(females)[18] <- "85+"
females <- cbind("females",females)
ESextr <- rbind(males,females)


males <- matrix(as.numeric(ESextr[1:18,2:5]),ncol=4)
females <- matrix(as.numeric(ESextr[19:36,2:5]),ncol=4)
widths <- rep(5,nrow(ESextr/2))

Pyramid(males,
		females,
		widths=widths,
		main = "Foreign population in Spain  (registered) by continent of bith, 2010",
		fill.males=gray(c(.8,.6,.4,.2)),
		fill.females=gray(c(.8,.6,.4,.2))
)
rect(1,70,2,90,col="white",border="black")
legend(1,90,legend=c("Europe","Africa","Americas","Asia"),fill=gray(c(.8,.6,.4,.2)),xpd=TRUE,bty="n")

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

# Myers blended index (indicator of age heaping / digit preference)
males <- cbind("males",males)
rownames(males)[18] <- "85+"
rownames(females)[18] <- "85+"
females <- cbind("females",females)
ESextr <- rbind(males,females)
PTpop <- PTpop
setwd("E:\\CODE\\PACKAGES\\PACKAGETEMP")
?package.skeleton
package.skeleton(name = "Pyramid",list=c("ESextr","PTpop","Pyramid","MyersI"))

getwd()

