evaluate.black <- function(blacks) {
	instruments <- instruments.optics
	n.blacks <- length(blacks)
	if(n.blacks > 0) {
		mymessage(paste("evaluating black ..."), head = "-")
		for(i in 1:n.blacks) {
			mymessage(paste("#", i, blacks[i]))
			data.black <- read.data(blacks[i])
			for(instr in instruments) {
				instr.median <- paste(instr, "median", sep = ".")
				if(i == 1) {
					assign(instr.median,  apply(data.black[[instr]], 2, median, na.rm = TRUE))
				} else {
					assign(instr.median,  get(instr.median) + apply(data.black[[instr]], 2, median, na.rm = TRUE))
				}
			}
		}
		ret <- list()
		for(instr in instruments) {
			instr.median <- paste(instr, "median", sep = ".")
			assign(instr.median,  get(instr.median) / n.blacks)
			ret <- c(ret, list(get(instr.median)))
		}
		names(ret) <- instruments
# PLOT
		if(INTERACTIVE) x11(width = win.width, height = win.height)
		par(mfrow = c(1, length(instruments)))
		for(instr in instruments) {
			instr.median <- ret[[instr]]
			plot(as.numeric(names(instr.median)), instr.median, xlab = expression(lambda ~~ "[nm]"), ylab = instr, main = paste("BLACK", instr))
			grid(col = 1)
		}
		par(mfrow = c(1, 1))
	} else {
		mymessage("no black", head = "-")
		ret <- NULL
	}


	ret
}
