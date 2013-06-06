#'
#'Calculate Myers' blended index for age heaping based on digit preference.
#'
#'Calculate Myers' blended index for age heaping based on digit preference.
#'Implemented as described in Shyrock et. al. (1980).
#'
#'Large numbers mean more bias in digit preference. Numbers less than 1 can be
#'disregarded. Interpret as percent of population that would need to be moved
#'to a different age category for there to be no noticeable pattern of
#'preference in age declaration.
#'
#'@param x vector of age-specific population counts
#'@param ages vector of completed ages.
#'@return the blended index value is returned.
#'@note This implementation will only work for digits 0-9: not months, zodiacs
#'or anything else that doesn't have a period of 10
#'@author Tim Riffe
#'@references Myers, R.J. (1940) Errors and bias in the reporting of ages in
#'census data. Transactions of the Actuarial Society of America. vol 41 nr 2
#'pages 395--415\cr Shryock, H.S. and Siegel, J.S. and Larmon, E.A. (1980) The
#'methods and materials of demography. US Dept. of Commerce, Bureau of the
#'Census (pages 116-118)
#'@examples
#'
#'data(PTpop)
#'
#'# separately for males and females in both years
#'apply(PTpop,2,MyersI)
#'
#'# males and females together, 1950
#'MyersI(rowSums(PTpop[,1:2]))
#'

MyersI <-
function(x,ages){
	if (missing(ages)) { 
		ages <- 0:(length(x)-1)
	}
	# ul = upper limit = largest age evenly divisible by 10 up to 100
	# i.e if highest age is 85, then it spits back 80
	ul <- min(c(max(ages[ages %% 10 == 0]), 100))
	# indices to pick out 2 tabulations
	ind1 <- ages >= 10 & ages < ul
	ind2 <- ages >= 20 & ages < ul
	# sum by digits, in this case picked out by modulars (%%)
	tab1 <- tapply(x[ind1],ages[ind1] %% 10,sum)
	tab2 <- tapply(x[ind2],ages[ind2] %% 10,sum)
	# weighted tabulation
	TAB <- tab1 * 1:10 + tab2 * 9:0
	# interpret as % that would need to be redistributed...
	sum(abs(TAB/sum(TAB) - .1)) * 50
}
