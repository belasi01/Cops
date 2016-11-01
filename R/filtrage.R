filtrage <- function(val, demifenetre = 1, delta = 0, npass = 1, fill = FALSE, replace = FALSE) {
	val2 <- array(NA, dim = dim(val))
	for(i in 1:ncol(val)) {
		val2[, i] <- filtre.mediane(demifenetre, log(val[, i]), delta = delta, fill = fill, replace = replace)
		if(npass > 1) {
			for(ipass in 1:(npass-1)) {
				val2[, i] <- filtre.mediane(demifenetre,     val2[, i], delta = delta, fill = fill, replace = FALSE)
			}
		}
	}
	exp(val2)
}
