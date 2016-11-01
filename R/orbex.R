orbex <- function(month,day) {
	ndays <- as.numeric(julian(
		as.POSIXct(paste("2001",month,day,sep = "-")),
		origin = as.POSIXct("2001-01-01")))
	return((1 + 0.0167 * cos(2*pi* (ndays -3) / 365))^2)
}
