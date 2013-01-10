Pyramid <-
		function(males,	females, widths, prop = TRUE,
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
	
	# for figuring out axes
	xmax1 <- if (is.null(dim(males))) {max(c(males, females)/widths)} else {max(c(rowSums(males), rowSums(females)/widths))}
	
	# should we round to 1000s or 1000000s for axis labels?
	KorM 							<- 10 ^ (3 * floor(log10(xmax1)/3))
	KorMTitle 						<- (10 ^ (3 * floor(log10(tot)/3)))
	KorMlab 						<- ifelse(KorMTitle == 1000, "(1000s)", "(millions)")
	
	# deciding proper x axes and labels in accordance with prop:
	if (prop){
		males 						<- (males/tot * -100)/widths # -% males (so that they plot to the left)
		females 					<- (females/tot * 100)/widths # % females, plotting to the right
		if (k == 1) {
			xmax 					<- max(c(-males, females))
		} else {
			xmax 					<- max(c(rowSums(-males), rowSums(females)))
			males 					<- t(males)
			females 				<- t(females)
		}
		if (missing(xlab)) {xlab 	<- "percent"}
	} else{
		males 						<- (-males/KorM)/widths
		females 					<- (females/KorM)/widths
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
	if (missing(ylim)) 					{ylim 				<- range(ages)+c(0,widths[length(widths)])}
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
		if (missing(border.females)) 	{border.females 	<- "black"}
	}
	if (k == 1){
		if (missing(fill.males)) 		{fill.males 		<- "orange"}
		if (missing(fill.females)) 		{fill.females 		<- "purple"}
		if (missing(border.males)) 		{border.males 		<- "transparent"}
		if (missing(border.females)) 	{border.females 	<- "transparent"}
	}
	
	# a default title, if year is not specified then 2000 is used, and the user ought to notice. This is so that
	# the user doesn't accidentally allow the generations to be calculated using the year 2000.
	if (missing(main)){ main <- paste("Total Pop = ",round(tot/KorMTitle,digits=1),KorMlab)}
	
	# default margins
	if (missing(mar))					{mar 				<- c(5,5,5,5) + 0.1}
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

