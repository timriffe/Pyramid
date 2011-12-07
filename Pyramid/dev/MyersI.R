
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


