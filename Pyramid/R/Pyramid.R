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
	# short verbose code
	Verb <- function(v, x){
		if (v==TRUE) {
			cat(paste(x, "\n", sep = ""))
		}
	}
	
	# check that male and female data are of equal length 
	if (length(males) != length(females)) {stop("the vectors input for males and females 
