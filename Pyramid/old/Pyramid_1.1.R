# version prior to 7 Dec 2011

Pyramid <-
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
		males <- (males/tot * -100)/widths # -% males (so that they plot to the left)
		females <- (females/tot * 100)/widths # % females, plotting to the right
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
	# age labels % or count labels
	if (missing(xaxat)) 		{xaxat 	<- pretty(c(-xmax,xmax),8)}
	if (missing(xax.lab)) 		{xax.lab 	<- abs(xaxat)}
	
	# age labels
	if (missing(yaxat)) 		{yaxat <- ages[ages%%10==0]}
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
		gen0 <- gen[gen%%10==0] # this is where the code figures out where the even numbered generations are (by 10)
		genat <- seq(from=max(gen)-max(gen0),to=max(gen)-min(gen),by=10) # where to plot the generations to track (in case of iterative plotting)
		axis(4,at=genat,labels=gen0,las=1)
		abline(h=genat,lty=gen.lty,col=gen.col,lwd=gen.lwd)
		mtext("generation",4,padj=4,cex=cex.lab)
	}
}

