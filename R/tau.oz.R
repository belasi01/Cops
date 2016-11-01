tau.oz <- function(wave, du = 350) {
	return(du / 1000 * approx(oz.k$wave,oz.k$oz,wave, rule = 2)$y)
}
