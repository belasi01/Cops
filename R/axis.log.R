axis.log <- function(side, nolabel = F, tics.int = T, grid = F, grid.int = F, ...) {
	args <- list(...)
	lims <- par("usr")
	if(side == 1 || side == 3) {
		min <- lims[1]
		max <- lims[2]
	}
	if(side == 2 || side == 4) {
		min <- lims[3]
		max <- lims[4]
	}
	x <- floor(min):ceiling(max)
	labs <- vector()
	i <- 0
	for (e in x) {
		i <- i + 1
#		labs[i] <- eval(substitute(expression(10^e), list(e = e)))
		labs[i] <- 10^e
	}
	if(nolabel) labs <- F
	axis(side, at = 10^x,
		col = args$col, col.axis = args$col.axis,
		cex.axis = args$cex.axis,
		labels = labs)
	if(grid & (side == 2 || side == 4)) abline(h = 10^x, lwd = 0.5, col = args$col)
	if(grid & (side == 1 || side == 3)) abline(v = 10^x, lwd = 0.5, col = args$col)
	if(tics.int) axis(side, at = 10^x %*% t(2:9),
		col = args$col, col.axis = args$col.axis,
		cex.axis = args$cex.axis,
		labels = F, tcl = par("tcl") / 2)
	if(grid.int & (side == 2 || side == 4)) abline(h = 10^x %*% t(2:9), lwd = 0.5, col = args$col)
	if(grid.int & (side == 1 || side == 3)) abline(v = 10^x %*% t(2:9), lwd = 0.5, col = args$col)
}
