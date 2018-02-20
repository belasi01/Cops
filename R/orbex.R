#'  Correction factor for Sun-Earth distance variation thoughout the year
#'
#'@param month is the month (1 to 12)
#'@param day is the day of the month (1-31)
#'
#'@return Returns a factor that multiply F0 to account for Sun-Earth distance variation at any given date.
#' This factor can change the irradiance up to 3.3\%.
#'
#'@author Bernard Gentilly
#'
orbex <- function(month,day) {
	ndays <- as.numeric(julian(
		as.POSIXct(paste("2001",month,day,sep = "-")),
		origin = as.POSIXct("2001-01-01")))
	return((1 + 0.0167 * cos(2*pi* (ndays -3) / 365))^2)
}
