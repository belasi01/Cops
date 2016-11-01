plot.init.info <- function(cops.init, cops.info) {
	towrite <- names(cops.init)
	plot(c(0,1), c(0,1), type = "n", axes = FALSE, ann = FALSE)
	x <- 0.01
	y <- 0.99
	for(tow in towrite) {
		y <- y - 0.04
		text(x, y, paste(tow, paste(cops.init[[tow]], collapse = " ")), adj = c(0, 0.5), cex = 0.7)
	}

	towrite <- names(cops.info)
	x <- 0.66
	y <- 0.99
	for(tow in towrite) {
		y <- y - 0.04
		if(tow != "blacks") {
			text(x, y, paste(tow, paste(cops.info[[tow]], collapse = " ")), adj = c(0, 0.5), cex = 0.7)
		} else {
			text(x, y, tow, adj = c(0, 0.5), cex = 0.7)
			blacks <- cops.info[[tow]]
			for(b in blacks) {
				y <- y - 0.04
				text(x, y, paste("     ", b), adj = c(0, 0.5), cex = 0.7)
			}
		}
	}
}
