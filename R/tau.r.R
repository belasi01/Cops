tau.r <- function(wave) {
	wavemum <- wave * 1.e-3
	return(8.524e-3 / wavemum^4 + 9.63e-5 / wavemum^6 + 1.1e-7 / wavemum^8)
}
