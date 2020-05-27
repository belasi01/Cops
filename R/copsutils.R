Q.and.f.factors2 <- function(waves, chl, sunzen) {
	Q.sun.nadir <- rep(pi,length(waves))
	Q.0 <- rep(pi,length(waves))
	f.sun <- rep(0.33,length(waves))
	f.0 <- rep(0.33,length(waves))
	if(! is.na(chl)) {
		i <- 0
		for(w in waves) {
			i <- i + 1
			conditions.Q <- c(w,sunzen,log(chl), 0, 0)
			Q.sun.nadir[i] <- popt.f.Q(conditions.Q)$Q
			conditions.Q <- c(w,0,log(chl), 0, 0)
			Q.0[i] <- popt.f.Q(conditions.Q)$Q
			conditions.f <- c(w,sunzen,log(chl))
			f.sun[i] <- popt.f.f(conditions.f)$f
			conditions.f <- c(w,0,log(chl))
			f.0[i] <- popt.f.f(conditions.f)$f
		}
	}
	list(Q.0 = Q.0, Q.sun.nadir = Q.sun.nadir, f.0 = f.0, f.sun = f.sun)
}
etirrwindow <- function(waves, window) {
	wthui <- thuillier.completed.by.AM0AM1$wave
	extr <- NULL
	halfwindow <- window / 2
	for(wave in waves) {
		ws <- wthui[wthui >= wave - halfwindow & wthui <= wave + halfwindow]
		es <- mean(etirr(ws))
		extr <- append(extr, es)
	}
	return(extr)
}
func.Ed0.th <- function(sunzen, month, day, waves, window) {
	om.aer <-  0.98
	F.aer <- 0.75
	tau.a <- 0.10
	t <- exp(-(
		0.5 * tau.r(waves) +
		tau.oz(waves, du = 350) +
		(1 - om.aer * F.aer) * tau.a) / cos(sunzen *pi/180))
	return(cos(sunzen * pi / 180) * t * etirrwindow(waves, window) * orbex(month,day))
}
waterspectra <- function() {
	waterfile <- paste(Sys.getenv("R_COPS_DATA_DIR"), "water_spectra.nc", sep = "/")
	nc <-  nc_open(waterfile)
	aw.nc <- ncvar_get(nc, "aw")
	bw.nc <- ncvar_get(nc, "bw")
	w.nc <- ncvar_get(nc, "wavelength")
	nc_close(nc)
	return(list(w = w.nc, aw = aw.nc, bw = bw.nc))
}
ABphy <- function() {
	# Melin-Sclep utilise les coeffs A et B de la table Bricaud 95
	# nouvelle table TABLE_Ap_Ep_Aphi_Ephi.dat
	wabnfile <- paste(Sys.getenv("R_COPS_DATA_DIR"), "TABLE_Ap_Ep_Aphi_Ephi.dat", sep = "/")
	wabn <- read.table(wabnfile, header = TRUE)
	wabn <- wabn[c("lambda", "Aphi", "Ephi")]
	# *** N.B. *** : dans cette nouvelle table,
	#   Aphy = A
	#   Ephi = 1 - B
	# en effet dans Bricaud 95 il s'agit de aphi* (absorption specifique)
	wab <- data.frame(w = wabn$lambda, A = wabn$Aphi, B = 1 - wabn$Ephi)
	cat("A-B table, Eq. 4 Melin Sclep\n")
	return(wab)
}
