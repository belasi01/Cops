process.EuZ <- function(cops.raw,
                        cops.dd,
                        cops.black,
                        cops.Ed0,
                        EXTRAPOLATION.0m = TRUE) {

	mymessage(paste("Processing EuZ", " ..."), head = "+")

	aop <- cops.raw$EuZ
	correction <- cops.Ed0$Ed0.correction
	aop <- aop * correction
	waves <- as.numeric(cops.raw$EuZ.waves)
	Depth <- cops.dd$Depth + delta.capteur.optics["EuZ"]
	depth.fitted <- cops.dd$depth.fitted
	if(!is.null(cops.dd[["EuZ.tilt"]]) && is.null(cops.dd[["EdZ.tilt"]])) {
		tilt <- cops.dd[["EuZ.tilt"]]
	} else {
		tilt <- cops.dd[["EdZ.tilt"]]
	}
	if(!is.null(cops.black)) {
		black <- cops.black$EuZ
		aop <- t(t(aop) - black)
	}

# tilt limitation
	valid.tilt <- tilt < tiltmax.optics["EuZ"]
	Depth.kept <- cops.dd$Depth.good & valid.tilt & Depth > sub.surface.removed.layer.optics["EuZ"]
	Depth <- Depth[Depth.kept]
	tilt <- tilt[Depth.kept]
	aop <- aop[Depth.kept, ]

	##### Remove radiometric measurement below the detection limit
	aop.all <- aop
	aop.all[aop.all < 0] <-0
	for (w in 1:19) {
	  aop[aop[,w] <= EuZ.detect.lim[w],w] <- NA
	}

# METHOD loess to fit this aop
	if (EXTRAPOLATION.0m) {
	  idx.depth.0 <- which.min(abs(depth.fitted - 0))
	  #  idx.depth.0 <- 1  ### Il me semble que ca donne la même chose...
	} else {
	  idx.depth.0 <- which.min(abs(depth.fitted - Depth[1])) # ice version
	}
	depth.0 <- depth.fitted[idx.depth.0]
	span <- depth.interval.for.smoothing.optics["EuZ"]
	fitted <- fit.with.loess(waves, Depth, log(aop), span, depth.fitted,
	                         idx.depth.0,
	                         span.wave.correction = TRUE, DEPTH.SPAN = TRUE ,
	                         minimum.obs = 10)
	aop.fitted <- exp(fitted$aop.fitted)
	aop.0 <- exp(fitted$aop.0)
  K <- compute.K(depth.fitted, idx.depth.0, aop.0, aop.fitted)
	KZ.fitted <- K$KZ.fitted
	K0.fitted <- K$K0.fitted

	#### Clean calculated AOP
	n.fitted <- length(depth.fitted)
	for (w in 1:19) {
	  print(paste("Clean AOP for EuZ ", waves[w]))
	  if (!all(is.na(aop.fitted[,w]))) { # if all NA, then the AOPs all ready equal to NA
	    # Apply a smooth.spline on raw data for further flaging on the AOP
	    tmp = smooth.spline(Depth, aop.all[,w])$y
	    aop.spline = spline(Depth, tmp,
	                        xout = depth.fitted, method = 'natural')$y
	    # remove bad data
	    KZ.fitted[(aop.fitted[(2:n.fitted),w] <= EuZ.detect.lim[w] |
	                 aop.spline[2:n.fitted]     <= EuZ.detect.lim[w]&
	                 depth.fitted[2:n.fitted] > Depth[1]),w] <- NA
	    K0.fitted[(aop.fitted[(2:n.fitted),w] <= EuZ.detect.lim[w] |
	                 aop.spline[2:n.fitted]   <= EuZ.detect.lim[w]&
	                 depth.fitted[2:n.fitted] > Depth[1]),w] <- NA
	    aop.fitted[(aop.fitted[,w]            <= EuZ.detect.lim[w] |
	                  aop.spline               <= EuZ.detect.lim[w]&
	                  depth.fitted > Depth[1]),w] <- NA
	  }
	  if (!is.na(aop.0[w])) {
	    if (aop.0[w] <= EuZ.detect.lim[w]) aop.0[w] <- NA
	  }
	}

	# Extrapolate EuZ to 0- using linear method
	K <- compute.Ksurf.linear(Depth, aop,
	                          instrument = "EuZ",
	                          delta.depth= 2.5,
	                          r2.threshold=0.6)
	r2 <- K$r2
	K.surf <- K$Kx
	Z.interval <- K$Z.interval
	ix.Z.interval <- K$ix.Z.interval
	if (EXTRAPOLATION.0m) {
	  EuZ.0m.linear <- K$X.0m
	} else {
	  EuZ.0m.linear <- K$X.0m * exp(-depth.0*K.surf)
	}
	KolmolSmirnov.p.value<-K$KolmolSmirnov.p.value
	EuZ.fitted <- matrix(NA, ncol=dim(aop.fitted)[2], nrow=dim(aop.fitted)[1])
	for (i in 1:length(waves)) {
	  if (!is.na(EuZ.0m.linear[i])) {
	    EuZ.fitted[idx.depth.0:n.fitted,i] <- K$X.0m[i] * exp(-depth.fitted[idx.depth.0:n.fitted]*K.surf[i])
	  }
	}

	# Compute PAR at depth
	PAR <- compute.PARz(depth.fitted, waves, aop.fitted, aop.0,
	                    f.PAR=c(0.01, 0.05))

# PLOT
	PLOT.LINEAR <- !all(is.na(EuZ.0m.linear))
	if (!PLOT.LINEAR) Z.interval=5

	aop.cols <- rainbow.modified(length(waves))
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(aop.all, Depth, type = "p", log = "x",
	        ylim = c(max(Z.interval,na.rm = T)+1,0),
	        xlim=c(min(EuZ.detect.lim),max(aop, na.rm=T)), pch = ".", cex = 1,
	        ylab="Depth (m)",
	        xlab = expression(E[u]*z* ~~ "("*mu*W.*cm^{-2}*.nm^{-1}*")"),
	        col = aop.cols)
	grid(col = 1)
	matplot(aop.fitted, depth.fitted, type = "l",
	        lty = 1, lwd = 2, add = TRUE, col = aop.cols)
	points(aop.0, rep(depth.0, length(aop.0)), col = aop.cols)
	if (PLOT.LINEAR) matplot(EuZ.fitted, depth.fitted, type = "l",
	        lty = 2, lwd = 2, add = TRUE, col = aop.cols)
	if (PLOT.LINEAR) points(diag(aop[ix.Z.interval,]), Z.interval, cex=1.5,
	       pch = 19,col = aop.cols)
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
	  if (length(which(!is.na(aop[,i]))) > 0) {
	    plot(aop.all[, i], Depth, type = "p", log = "x",
	         xlim = range(aop[aop[, i] > 0, i], na.rm = TRUE),
	         ylim = rev(range(Depth, depth.fitted)),
	         pch = ".",
	         xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE,
	         main = substitute(E[u]*z~x~"("*mu*W.*cm^{-2}*.nm^{-1}*")",list(x = waves[i])))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = 2)
	    points(aop.0[i], depth.0, pch = 20, col = 4)
	    abline(v=EuZ.detect.lim[i], col="orange", lwd=2)
	    abline(h=sub.surface.removed.layer.optics["EuZ"], col="green", lwd=1)
	    axis(1)
	    axis(2)
	  }
	}
	par(mai = mai.old2)
	par(mgp = mgp.old2)
	par(mfrow = c(1, 1))
	par(mai = mai.old1)
	par(mgp = mgp.old1)

	##### check surface interpolation
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	mai.old1 <- par("mai")
	mgp.old1 <- par("mgp")
	par(mfrow = c(4, 5))
	par(mai = par("mai") / 2, mgp = par("mgp") / 2)
	mai.old2 <- par("mai")
	mgp.old2 <- par("mgp")
	for(i in 1:length(waves)) {
	  if (length(which(!is.na(aop[,i]))) > 0) {
	    plot(aop.all[, i], Depth, type = "p", log = "x",
	         xlim = range(aop[, i], aop.0[i], na.rm = TRUE),
	         ylim = c(max(Z.interval,na.rm = T)+0.5,0),
	         pch = ".", xlab = "", ylab = "",
	         axes = FALSE, frame.plot = TRUE,
	         main = substitute(E[u]*z~x~r^2==r.2,list(x = waves[i], r.2=signif(r2[i],3))))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = "red")
	    points(aop.0[i], depth.0, pch = 1, cex=1.5, col = "red")
	    if (PLOT.LINEAR) lines(EuZ.fitted[, i], depth.fitted, col = "blue")
	    if (PLOT.LINEAR) points(EuZ.0m.linear[i],depth.0, pch = 1, cex=1.5, col = "blue")
	    if (PLOT.LINEAR) points(aop[ix.Z.interval[i],i], Z.interval[i], cex=1.5,
	           pch = 19,col ="blue")
	    abline(v=EuZ.detect.lim[i], col="orange", lwd=2)
	    abline(h=sub.surface.removed.layer.optics["EuZ"], col="green", lwd=1)
	    axis(1)
	    axis(2)
	  }
	}
	par(mai = mai.old2)
	par(mgp = mgp.old2)
	par(mfrow = c(1, 1))
	par(mai = mai.old1)
	par(mgp = mgp.old1)

	##### check surface interpolation at selected wavelengths
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	mai.old1 <- par("mai")
	mgp.old1 <- par("mgp")
	par(mfrow = c(2, 2))
	par(mai = par("mai") / 2, mgp = par("mgp") / 2)
	mai.old2 <- par("mai")
	mgp.old2 <- par("mgp")
	#### Select 4 wavelengths
	if (PLOT.LINEAR) {
	  ix.w = which(!is.na(r2))
	} else ix.w <- 1:19
	for(i in floor(seq(ix.w[1], ix.w[length(ix.w)], length.out = 4))) {
	  if (length(which(!is.na(aop[,i]))) > 0) {
	    plot(aop.all[, i], Depth, type = "p", log = "x",
	         xlim = range(aop[, i], aop.0[i], na.rm = TRUE),
	         ylim = c(max(Z.interval,na.rm = T)+0.5,0),
	         pch = 19, xlab = "", ylab = "",
	         axes = FALSE, frame.plot = TRUE,
	         main = substitute(E[u]*z~x~"("*mu*W.*cm^{-2}*.nm^{-1}*")"~r^2==r.2,list(x = waves[i], r.2=signif(r2[i],3))))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = 2, lwd=2)
	    points(aop.0[i], depth.0, pch = 1, cex=1.8, col = "red")
	    if (PLOT.LINEAR) lines(EuZ.fitted[, i], depth.fitted, col = "blue")
	    if (PLOT.LINEAR) points(EuZ.0m.linear[i],depth.0, pch = 1, cex=1.8, col = "blue")
	    if (PLOT.LINEAR) points(aop[ix.Z.interval[i],i], Z.interval[i], cex=1.8,
	           pch = 19,col ="blue")
	    abline(v=EuZ.detect.lim[i], col="orange", lwd=3)
	    abline(h=sub.surface.removed.layer.optics["EuZ"], col="green", lwd=2)
	    if (i == floor(seq(ix.w[1], ix.w[length(ix.w)], length.out = 4))[4])
	      legend("bottomright", c("LOESS", "Linear"),
	             lwd=c(3,3), col=c("red", "blue"))
	    axis(1)
	    axis(2)
	  }
	}
	par(mai = mai.old2)
	par(mgp = mgp.old2)
	par(mfrow = c(1, 1))
	par(mai = mai.old1)
	par(mgp = mgp.old1)


	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(KZ.fitted, depth.fitted[-1], type = "l",
	        lty = 1, ylim = rev(range(Depth)),
	        xlab = expression(K[z]~"("*E[u]*"z) (local K, i.e. derivative of Eu(z))"),
	        ylab = "Depth (m)",col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(K0.fitted, depth.fitted[-1], type = "l",
	        lty = 1, ylim = rev(range(Depth)),
	        xlab = expression(K[0]~"("*E[u]*"z) (Depth-integrated from 0- to Z"),
	        ylab = "Depth (m)", col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	if (!PLOT.LINEAR) Z.interval <-NA
	return(list(
		EuZ.fitted = aop.fitted,
		KZ.EuZ.fitted = KZ.fitted,
		K0.EuZ.fitted = K0.fitted,
		EuZ.0m = aop.0,
		K.EuZ.surf = K.surf,
		EuZ.Z.interval = Z.interval,
		EuZ.ix.Z.interval = ix.Z.interval,
		EuZ.0m.linear = EuZ.0m.linear,
		EuZ.linear.r2 = r2,
		EuZ.KolmolSmirnov.p.value = KolmolSmirnov.p.value,
		EuZ.detection.limit = EuZ.detect.lim,
		z.f.PARu       = PAR$z.f.PAR,
		PARu.z         = PAR$PAR.z,
		PARu.at.z      = PAR$PAR.at.z

	))
}
