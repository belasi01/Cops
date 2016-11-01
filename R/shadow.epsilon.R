shadow.epsilon <- function(instr, aR, sunzen, ratio.edsky.edsun) {
	table.sun.name <- paste("table.sun", instr, sep = ".")
	table.sun.value <- GordonDing.d[[table.sun.name]]
	tan.sunzen.water <- tan(asin(sin(sunzen*pi/180)/1.33))
	kprime.sun <- approx(GordonDing.d$t, table.sun.value, sunzen)$y / tan.sunzen.water
	table.sky.name <- paste("table.sky", instr, sep = ".")
	table.sky.value <- GordonDing.d[[table.sky.name]]
	kprime.sky <- table.sky.value
	eps.sun <- 1 - exp(-kprime.sun * aR)
	eps.sky <- 1 - exp(-kprime.sky * aR)
	ratio.lutruesky.lutruesun <- 1
	ratio <- ratio.edsky.edsun * ratio.lutruesky.lutruesun
	eps <- (eps.sun + eps.sky * ratio) / (1 + ratio)
	return(list(eps.sun = eps.sun, eps.sky = eps.sky, eps = eps))
}
