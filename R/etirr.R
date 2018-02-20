#'  Extraterrestrial Solar Irradiance (F0) from Thuillier
#'
#'@param waves is a vector for wavelengths
#'
#'@return Returns F0 values in W/m^2/nm
#'
#'@author Bernard Gentilly
#'
etirr <- function(waves) {
	return(0.1 * approx(thuillier.completed.by.AM0AM1$wave,thuillier.completed.by.AM0AM1$F0,waves)$y)
}
