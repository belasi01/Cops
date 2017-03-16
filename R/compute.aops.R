compute.aops <- function(cops) {

####################
	mymessage("Computing derived AOPs ...", head = "+", tail = "-")

####################
# down AOPs
	waves.d <- cops$EdZ.waves
	Ed0.0p <- cops$Ed0.0p

####################
# up   AOPs + SHADOW.CORRECTION
	shadow.coef.EuZ <- NULL
	shadow.coef.LuZ <- NULL
	if("EuZ" %in% instruments.optics) {
		waves.u <- cops$EuZ.waves
		EuZ.0m <- cops$EuZ.0m
		EuZ.0m.linear <- cops$EuZ.0m.linear
		if(cops$SHADOW.CORRECTION) {
			shadow.coef.EuZ <- shadow.correction("EuZ", cops)
			EuZ.0m <- EuZ.0m / shadow.coef.EuZ$EuZ.shad.correction
			EuZ.0m.linear <- cops$EuZ.0m.linear / shadow.coef.EuZ$EuZ.shad.correction
		}
		LuZ.0m <- EuZ.0m / cops$Q.sun.nadir
		LuZ.0m.linear <- EuZ.0m.linear / cops$Q.sun.nadir
	}
	if("LuZ" %in% instruments.optics) {
		waves.u <- cops$LuZ.waves
		LuZ.0m <- cops$LuZ.0m
		LuZ.0m.linear <- cops$LuZ.0m.linear
		if(cops$SHADOW.CORRECTION) {
			shadow.coef.LuZ <- shadow.correction("LuZ", cops)
			LuZ.0m <- LuZ.0m / shadow.coef.LuZ$LuZ.shad.correction
			LuZ.0m.linear <- LuZ.0m.linear / shadow.coef.LuZ$LuZ.shad.correction
		}
		EuZ.0m <- LuZ.0m * cops$Q.sun.nadir
		EuZ.0m.linear <- LuZ.0m.linear * cops$Q.sun.nadir
	}
####################

	mymessage("Computing Lw.0p ...", head = "-")
	Lw.0p <- LuZ.0m * (1 - rau.Fresnel) / indice.water^2
	Lw.0p.linear <- LuZ.0m.linear * (1 - rau.Fresnel) / indice.water^2

	mymessage("Computing nLw.0p ...", head = "-")
	nLw.0p <- Lw.0p / Ed0.0p * etirr(waves.u)
	nLw.0p.linear <- Lw.0p.linear / Ed0.0p * etirr(waves.u)

	mymessage("Computing Rrs.0p ...", head = "-")
	Rrs.0p <- Lw.0p / Ed0.0p
	Rrs.0p.linear <- Lw.0p.linear / Ed0.0p

	mymessage("Computing Ed0.0m ...", head = "-")
	Ed0.0m <- 0.96 * Ed0.0p

	mymessage("Computing R.0m ...", head = "-")
	R.0m <- EuZ.0m / Ed0.0m
	R.0m.linear <- EuZ.0m.linear / Ed0.0m

	mymessage("Computing Forel-Ule Color ...", head = "-")
	FU <- Rrs2FU(waves.u, Rrs.0p)$FU
	FU.linear <- Rrs2FU(waves.u, Rrs.0p.linear)$FU

# PLOT
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	par(mfrow = c(2, 3))
	plot(1, 1, type = "n", log = "y", xlim = range(waves.u), ylim = c(0.00005, 10), xlab = expression(lambda ~~ nm), ylab = expression(L[w]), axes = FALSE, frame.plot = TRUE)
	axis(1)
	axis.log(2, grid = TRUE, col = 1, lwd = 0.5, lty = 2)
	lines(waves.u, Lw.0p, type = "b")
	plot(1, 1, type = "n", log = "y", xlim = range(waves.u), ylim = c(0.00005, 10), xlab = expression(lambda ~~ nm), ylab = expression(nL[w]), axes = FALSE, frame.plot = TRUE)
	axis(1)
	axis.log(2, grid = TRUE, col = 1, lwd = 0.5, lty = 2)
	lines(waves.u, nLw.0p, type = "b")
	plot(1, 1, type = "n", log = "y", xlim = range(waves.u), ylim = c(0.000005, 0.1), xlab = expression(lambda ~~ nm), ylab = expression(R[0*"-"]), axes = FALSE, frame.plot = TRUE)
	axis(1)
	axis.log(2, grid = TRUE, col = 1, lwd = 0.5, lty = 2)
	lines(waves.u, R.0m, type = "b")
	plot(1, 1, type = "n", log = "y", xlim = range(waves.u), ylim = c(0.000005, 0.1), xlab = expression(lambda ~~ nm), ylab = expression(Rrs[0*"+"]), axes = FALSE, frame.plot = TRUE)
	axis(1)
	axis.log(2, grid = TRUE, col = 1, lwd = 0.5, lty = 2)
	lines(waves.u, Rrs.0p, type = "b")
	if(!is.null(shadow.coef.EuZ)) {
		if(is.na(cops$chl)) {
			shadow.correction.type <- "measured absorption"
		} else {
			shadow.correction.type <- "absorption from chlorophyll\ncase 1 waters model"
		}
		plot(waves.u, shadow.coef.EuZ$EuZ.shad.correction, type = "b", xlim = range(waves.u), ylim = c(0.2, 1), xlab = expression(lambda ~~ nm), ylab = "shadow correction for EuZ", main = shadow.correction.type)
		abline(h = seq(0.2, 1, 0.1), lty = 3)
	} else {
		if("EuZ" %in% cops$instruments.optics) {
			plot(1, 1, type = "n", axes = FALSE, ann = FALSE)
			text(1, 1, "NO SHADOW CORRECTION FOR EuZ", adj = c(0.5, 0.5))
		}
	}
	if(!is.null(shadow.coef.LuZ)) {
		if(is.na(cops$chl)) {
			shadow.correction.type <- "measured absorption"
		} else {
			shadow.correction.type <- "absorption from chlorophyll\ncase 1 waters model"
		}
		plot(waves.u, shadow.coef.LuZ$LuZ.shad.correction, type = "b", xlim = range(waves.u), ylim = c(0.2, 1), xlab = expression(lambda ~~ nm), ylab = "shadow correction for LuZ", main = shadow.correction.type)
		abline(h = seq(0.2, 1, 0.1), lty = 3)
	} else {
		if("LuZ" %in% cops$instruments.optics) {
			plot(1, 1, type = "n", axes = FALSE, ann = FALSE)
			text(1, 1, "NO SHADOW CORRECTION FOR LuZ", adj = c(0.5, 0.5))
		}
	}
	par(mfrow = c(1, 1))

	if(!is.null(shadow.coef.EuZ) & !is.null(shadow.coef.LuZ)) shadow.list <- c(shadow.coef.EuZ, shadow.coef.EuZ)
	if(!is.null(shadow.coef.EuZ) &  is.null(shadow.coef.LuZ)) shadow.list <- shadow.coef.EuZ
	if( is.null(shadow.coef.EuZ) & !is.null(shadow.coef.LuZ)) shadow.list <- shadow.coef.LuZ
	if( is.null(shadow.coef.EuZ) &  is.null(shadow.coef.LuZ)) shadow.list <- NULL
	return(c(shadow.list, list(
		Lw.0p = Lw.0p,
		nLw.0p = nLw.0p,
		R.0m = R.0m,
		Rrs.0p = Rrs.0p,
		Lw.0p.linear = Lw.0p.linear,
		nLw.0p.linear = nLw.0p.linear,
		R.0m.linear = R.0m.linear,
		Rrs.0p.linear = Rrs.0p.linear,
		FU = FU,
		FU.linear = FU.linear
	)))
}
