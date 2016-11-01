etirr <- function(wave) {
	return(0.1 * approx(thuillier.completed.by.AM0AM1$wave,thuillier.completed.by.AM0AM1$F0,wave)$y)
}
