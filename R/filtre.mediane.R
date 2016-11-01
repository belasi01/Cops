filtre.mediane <- function(k, y, delta = 0, fill = TRUE, replace = TRUE) {
	f <- y
	y[is.infinite(f)] <- NA
	if(length(f) < 2 * k + 2) return(f)
	n <- length(y)
	i1 <- k + 1
	i2 <- n - k

	for (i in i1:i2) {
		w <- y[(i - k):(i + k)]
		if(!is.na(y[i]) & !all(is.na(w))) {
			w.m <- median(w, na.rm = TRUE)
			if(abs(y[i] - w.m) > delta & replace) f[i] <- w.m
			if(abs(y[i] - w.m) > delta & (! replace)) f[i] <- NA
		}
	}
	if(! fill) {
		f[1:k] <- NA
		f[(i2+1):n] <- NA
	}
	return(f)
}
