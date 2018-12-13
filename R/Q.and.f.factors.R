#' Retrieve the Q factor
Q.and.f.factors <- function(cops.info, cops.raw, cops.dd, cops.EuZ, cops.LuZ) {
	waves <- NULL
  if(is.null(cops.EuZ)) waves <- cops.raw$LuZ.waves
	if(is.null(cops.LuZ)) waves <- cops.raw$EuZ.waves
	if(is.null(waves)) waves <- cops.raw$EuZ.waves # Added by Simon Belanger on Nov 30th 2018 to handle COPS data having both EuZ and LuZ
	Q.sun.nadir <- rep(pi,length(waves))
	Q.0 <- rep(pi,length(waves))
	f.sun <- rep(0.33,length(waves))
	f.0 <- rep(0.33,length(waves))
	chl <- cops.info$chl
	if(! is.na(chl)) {
		sunzen <- cops.dd$sunzen
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

