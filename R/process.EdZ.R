process.EdZ <- function(cops.raw, cops.dd, cops.black, cops.Ed0) {

	mymessage(paste("Processing EdZ", " ..."), head = "+")

	aop <- cops.raw$EdZ
	correction <- cops.Ed0$Ed0.correction
	aop <- aop * correction
	waves <- as.numeric(cops.raw$EdZ.waves)
	Depth <- cops.dd$Depth
	depth.fitted <- cops.dd$depth.fitted
	if(!is.null(cops.dd[["EdZ.tilt"]])) {
		tilt <- cops.dd[["EdZ.tilt"]]
	} else {
		tilt <- cops.dd[["EuZ.tilt"]]
	}
	if(!is.null(cops.black)) {
		black <- cops.black$EdZ
		aop <- t(t(aop) - black)
	}
	aop[aop <= 0] <- NA

# tilt limitation
	valid.tilt <- tilt < tiltmax.optics["EdZ"]

	Depth.kept <- cops.dd$Depth.good & valid.tilt & Depth > sub.surface.removed.layer.optics["EdZ"]

	aop <- aop[Depth.kept, ]
	Depth <- Depth[Depth.kept] + delta.capteur.optics["EdZ"]
	tilt <- tilt[Depth.kept]

# METHOD loess to fit this aop
	span <- time.interval.for.smoothing.optics["EdZ"] / cops.dd$cops.duration.secs
	fitted <- fit.with.loess(waves, Depth, log(aop), span, depth.fitted, span.wave.correction = TRUE)
	aop.fitted <- exp(fitted$aop.fitted)
	aop.0 <- exp(fitted$aop.0)
	K <- compute.K(depth.fitted, aop.0, aop.fitted)
	KZ.fitted <- K$KZ.fitted
	K0.fitted <- K$K0.fitted

	# Extrapolate EdZ to 0- using linear method
	K <- compute.Ksurf.linear(Depth, aop,
	                          r2.threshold = 0.99,
	                          detect.lim = 1e-4)
	K.surf <- K$Kx
	Z.interval <- K$Z.interval
	EdZ.0m.linear <- K$X.0m

# PLOT
	aop.cols <- rainbow.modified(length(waves))
	if(INTERACTIVE) x11(width = win.width, height = win.height)
#	matplot(aop, Depth, type = "p", log = "x", ylim = rev(range(c(0, Depth))), pch = ".", cex = 1, xlab = "EdZ", col = aop.cols)
  matplot(aop, Depth, type = "p", log = "x", ylim = rev(range(c(0, Depth))),xlim=c(1e-5,max(aop, na.rm=T)), pch = ".", cex = 1, xlab = "EdZ", col = aop.cols)
  grid(col = 1)
	matplot(aop.fitted, depth.fitted, type = "l", lty = 1, lwd = 2, add = TRUE, col = aop.cols)
	points(aop.0, rep(0, length(aop.0)), col = aop.cols)
	par(xpd = TRUE)
	legend(10^par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	mai.old1 <- par("mai")
	mgp.old1 <- par("mgp")
	par(mfrow = c(4, 5))
	par(mai = par("mai") / 2, mgp = par("mgp") / 2)
	mai.old2 <- par("mai")
	mgp.old2 <- par("mgp")
	for(i in 1:length(waves)) {
		plot(aop[, i], Depth, type = "p", log = "x", xlim = range(aop[, i], aop.0[i], na.rm = TRUE), ylim = rev(range(Depth, depth.fitted)), pch = ".", xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE, main = paste("EdZ", waves[i]))
		grid(col = 1)
		lines(aop.fitted[, i], depth.fitted, col = 2)
		points(aop.0[i], 0, pch = 20, col = 4)
		axis(1)
		axis(2)
	}
	par(mai = mai.old2)
	par(mgp = mgp.old2)
	par(mfrow = c(1, 1))
	par(mai = mai.old1)
	par(mgp = mgp.old1)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(KZ.fitted, depth.fitted[-1], type = "l", lty = 1, ylim = rev(range(Depth)), xlab = "KZ.EdZ", col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(K0.fitted, depth.fitted[-1], type = "l", lty = 1, ylim = rev(range(Depth)), xlab = "K0.EdZ", col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	return(list(
		EdZ.fitted = aop.fitted,
		KZ.EdZ.fitted = KZ.fitted,
		K0.EdZ.fitted = K0.fitted,
		EdZ.0m = aop.0,
		K.EdZ.surf = K.surf,
		EdZ.Z.interval = Z.interval,
		EdZ.0m.linear = EdZ.0m.linear
	))
}
