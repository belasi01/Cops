process.Ed0 <- function(cops.raw, cops.dd, cops.black) {

	mymessage(paste("Processing Ed0", " ..."), head = "+")

	aop.raw <- cops.raw$Ed0
	aop <- aop.raw
	waves <- as.numeric(cops.raw$Ed0.waves)
	Depth.raw <- cops.dd$Depth
	Depth <- Depth.raw
	depth.fitted <- cops.dd$depth.fitted
	tilt <- cops.dd[["Ed0.tilt"]]
	sunzen <- cops.dd$sunzen
	month <- cops.dd$month
	day <- cops.dd$day
	if(!is.null(cops.black)) {
		black <- cops.black$Ed0
		aop <- t(t(aop) - black)
	}

# tilt limitation
	valid.tilt <- tilt < tiltmax.optics["Ed0"]

	Depth.kept <- cops.dd$Depth.good & valid.tilt

	aop <- aop[Depth.kept, ]
	Depth <- Depth[Depth.kept]
	tilt <- tilt[Depth.kept]

# METHOD loess to fit this aop
	span <- depth.interval.for.smoothing.optics["Ed0"]
	fitted <- fit.with.loess(waves, Depth, aop, span,
	                         depth.fitted,
	                         span.wave.correction = FALSE,
	                         DEPTH.SPAN=TRUE)
	aop.fitted <- fitted$aop.fitted
	aop.0 <- fitted$aop.0
	E0d.correction <- t(aop.0 / t(aop.raw))

# theorical Ed0
	om.aer <-  0.98
	F.aer <- 0.75
	tau.a <- 0.10
	t <- exp(-(
		0.5 * tau.r(waves) +
		tau.oz(waves, du = 350) +
		(1 - om.aer * F.aer) * tau.a) / cos(sunzen *pi/180))
	Ed0.th <- cos(sunzen * pi / 180) * t * etirrwindow(waves, bandwidth) * orbex(month,day)

	aop.cols <- rainbow.modified(length(waves))
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(aop, Depth, type = "p", ylim = rev(range(Depth)),
	        pch = ".", cex = 1,
	        ylab="Depth (m)",
	        xlab = expression(E[surface]* ~~ "("*mu*W.*cm^{-2}*.nm^{-1}*")"), col = aop.cols)
	grid(col = 1)
	matplot(aop.fitted, depth.fitted, type = "l", lty = 1, lwd = 2, add = TRUE, col = aop.cols)
	points(aop.0, rep(0, length(aop.0)), col = aop.cols, pch = 20, cex = 2)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	return(list(
		Ed0.th = Ed0.th,
		Ed0.0p = aop.0,
		Ed0.fitted = aop.fitted,
		Ed0.correction = E0d.correction
	))
}
