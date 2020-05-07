process.LuZ <- function(cops.raw,
                        cops.dd,
                        cops.black,
                        cops.Ed0,
                        EXTRAPOLATION.0m = TRUE) {

	mymessage(paste("Processing LuZ", " ..."), head = "+")

  temperature <-cops.raw$LuZ.anc$Temp[cops.dd$Depth.good]
  temperature.depth <-cops.raw$LuZ.anc$Depth[cops.dd$Depth.good]

	aop <- cops.raw$LuZ
	correction <- cops.Ed0$Ed0.correction
	aop <- aop * correction
	waves <- as.numeric(cops.raw$LuZ.waves)
	Depth <- cops.dd$Depth + delta.capteur.optics["LuZ"]
	depth.fitted <- cops.dd$depth.fitted
	if(!is.null(cops.dd[["LuZ.tilt"]]) && is.null(cops.dd[["EdZ.tilt"]])) {
		tilt <- cops.dd[["LuZ.tilt"]]
	} else {
	  if (!is.null(cops.dd[["EdZ.tilt"]])) {
	    tilt <- cops.dd[["EdZ.tilt"]]
	  } else {
	    if (!is.null(cops.dd[["EuZ.tilt"]])) tilt <- cops.dd[["EuZ.tilt"]]
	  }
	}
	if(!is.null(cops.black)) {
		black <- cops.black$LuZ
		aop <- t(t(aop) - black)
	}

# tilt limitation
	valid.tilt <- tilt < tiltmax.optics["LuZ"]
	aop.all <- aop[cops.dd$Depth.good & valid.tilt,] # make a backup
	Depth.all <- Depth[cops.dd$Depth.good & valid.tilt]
	Depth.kept <- cops.dd$Depth.good & valid.tilt & Depth > sub.surface.removed.layer.optics["LuZ"]
	Depth <- Depth[Depth.kept]
	tilt <- tilt[Depth.kept]
	aop <- aop[Depth.kept, ]

	##### Remove radiometric measurement below the detection limit
	aop.all[aop.all < 0] <-0
	for (w in 1:19) {
	  aop[aop[,w] <= LuZ.detect.lim[w],w] <- NA
	}

# METHOD loess to fit this aop
	if (EXTRAPOLATION.0m) {
	  idx.depth.0 <- which.min(abs(depth.fitted - 0))
	  #  idx.depth.0 <- 1  ### Il me semble que ca donne la même chose...
	} else {
	  idx.depth.0 <- which.min(abs(depth.fitted - Depth[1])) # ice version
	}
	depth.0 <- depth.fitted[idx.depth.0]
	span <- depth.interval.for.smoothing.optics["LuZ"]
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
	  print(paste("Clean AOP for LuZ ", waves[w]))
	  if (!all(is.na(aop.fitted[,w]))) { # if all NA, then the AOPs all ready equal to NA
	    # Apply a spline on raw data for further flaging on the AOP
	    tmp = smooth.spline(Depth.all, aop.all[,w])
	    aop.spline = spline(tmp, xout = depth.fitted, method = 'natural')$y
	    # remove bad data
	    KZ.fitted[(aop.fitted[(2:n.fitted),w] <= LuZ.detect.lim[w] |
	               aop.spline[2:n.fitted]     <= LuZ.detect.lim[w]&
	                 depth.fitted[2:n.fitted] > Depth[1]),w] <- NA
	    K0.fitted[(aop.fitted[(2:n.fitted),w] <= LuZ.detect.lim[w] |
	                 aop.spline[2:n.fitted]   <= LuZ.detect.lim[w]&
	                 depth.fitted[2:n.fitted] > Depth[1]),w] <- NA
	    aop.fitted[(aop.fitted[,w]            <= LuZ.detect.lim[w] |
	                 aop.spline               <= LuZ.detect.lim[w]&
	                  depth.fitted > Depth[1]),w] <- NA
	  }
	  if (!is.na(aop.0[w])) {
	    if (aop.0[w] <= LuZ.detect.lim[w]) aop.0[w] <- NA
	  }
	}


	# Extrapolate Luz to 0- using linear method
	K <- compute.Ksurf.linear(Depth, aop,
	                          instrument = "LuZ",
	                          delta.depth= 2.5,
	                          r2.threshold=0.6)
	  r2 <- K$r2
	  K.surf <- K$Kx
	  Z.interval <- K$Z.interval
	  ix.Z.interval <- K$ix.Z.interval
	  if (EXTRAPOLATION.0m) {
	    LuZ.0m.linear <- K$X.0m
	  } else {
	    LuZ.0m.linear <- K$X.0m * exp(-depth.0*K.surf)
	  }
	  KolmolSmirnov.p.value<-K$KolmolSmirnov.p.value
	  LuZ.fitted <- matrix(NA, ncol=dim(aop.fitted)[2], nrow=dim(aop.fitted)[1])
	  for (i in 1:length(waves)) {
	    if (!is.na(LuZ.0m.linear[i])) {
	      LuZ.fitted[idx.depth.0:n.fitted,i] <- K$X.0m[i] * exp(-depth.fitted[idx.depth.0:n.fitted]*K.surf[i])
	    }
	  }

# Fit temperature profile
  actual.span=min(1,1/diff(range(temperature.depth)))
	fit.func <- loess(temperature ~ temperature.depth, span = actual.span,
	                    control = loess.control(surface = "direct"))
	temperature.fitted <- predict(fit.func, depth.fitted[idx.depth.0:n.fitted])

# PLOT
	PLOT.LINEAR <- !all(is.na(LuZ.0m.linear))
	if (!PLOT.LINEAR) Z.interval=5

	aop.cols <- rainbow.modified(length(waves))
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(aop.all, Depth.all, type = "p", log = "x",
          ylim = c(max(Z.interval,na.rm = T)+1,0),
	        xlim=c(min(LuZ.detect.lim),max(aop, na.rm=T)), pch = ".", cex = 1,
	        ylab="Depth (m)",
	        xlab = expression(L[u]*z* ~~ "("*mu*W.*cm^{-2}*.nm^{-1}*.sr^{-1}*")"),
          col = aop.cols)
  grid(col = 1)
	matplot(aop.fitted, depth.fitted, type = "l",
	        lty = 1, lwd = 2, add = TRUE, col = aop.cols)
	points(aop.0, rep(depth.0, length(aop.0)), col = aop.cols)
	if (PLOT.LINEAR) matplot(LuZ.fitted, depth.fitted, type = "l",
	        lty = 2, lwd = 2, add = TRUE, col = aop.cols)
	if (PLOT.LINEAR) points(diag(aop[ix.Z.interval,]), Z.interval, cex=1.5,
	       pch = 19,col = aop.cols)
	par(xpd = TRUE)
	legend(10^par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)
	if (!PLOT.LINEAR) text(aop.0[8], 0, "LINEAR INTERPOLATION FAILED, SHOULD YOU RELAX THE TILT THRESOLD?", pos=4)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	mai.old1 <- par("mai")
	mgp.old1 <- par("mgp")
	par(mfrow = c(4, 5))
	par(mai = par("mai") / 2, mgp = par("mgp") / 2)
	mai.old2 <- par("mai")
	mgp.old2 <- par("mgp")
	for(i in 1:length(waves)) {
	  if (length(which(!is.na(aop[,i]))) > 0) {
	    plot(aop.all[, i], Depth.all, type = "p", log = "x",
	         xlim = range(aop[aop[, i] > 0, i], na.rm = TRUE),
	         ylim = rev(range(Depth, depth.fitted)),
	         pch = ".",
	         xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE,
	         main = substitute(L[u]*z~x~"("*mu*W.*cm^{-2}*.nm^{-1}*.sr^{-1}*")",list(x = waves[i])))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = 2)
	    points(aop.0[i], depth.0, pch = 20, col = 4)
	    if (PLOT.LINEAR) abline(v=LuZ.detect.lim[i], col="orange", lwd=2)
	    abline(h=sub.surface.removed.layer.optics["LuZ"], col="green", lwd=1)
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
	    plot(aop.all[, i], Depth.all, type = "p", log = "x",
	         xlim = range(aop[, i], aop.0[i], na.rm = TRUE),
	         ylim = c(max(Z.interval,na.rm = T)+0.5,0),
	         pch = ".", xlab = "", ylab = "",
	         axes = FALSE, frame.plot = TRUE,
	         main = substitute(L[u]*z~x~r^2==r.2,list(x = waves[i], r.2=signif(r2[i],3))))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = "red")
	    points(aop.0[i], depth.0, pch = 1, cex=1.5, col = "red")
	    if (PLOT.LINEAR)  lines(LuZ.fitted[, i], depth.fitted, col = "blue")
	    if (PLOT.LINEAR)  points(LuZ.0m.linear[i],depth.0, pch = 1, cex=1.5, col = "blue")
	    if (PLOT.LINEAR)  points(aop[ix.Z.interval[i],i], Z.interval[i], cex=1.5,
	           pch = 19,col ="blue")
	    abline(v=LuZ.detect.lim[i], col="orange", lwd=2)
	    abline(h=sub.surface.removed.layer.optics["LuZ"], col="green", lwd=1)
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
	    plot(aop.all[, i], Depth.all, type = "p", log = "x",
	         xlim = range(aop[, i], aop.0[i], na.rm = TRUE),
	         ylim = c(max(Z.interval,na.rm = T)+0.5,0),
	         pch = 19, xlab = "", ylab = "",
	         axes = FALSE, frame.plot = TRUE,
	         main = substitute(L[u]*z~x~"("*mu*W.*cm^{-2}*.nm^{-1}*.sr^{-1}*")"~r^2==r.2,list(x = waves[i], r.2=signif(r2[i],3))))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = 2, lwd=2)
	    points(aop.0[i], depth.0, pch = 1, cex=1.8, col = "red")
	    if (PLOT.LINEAR)  lines(LuZ.fitted[, i], depth.fitted, col = "blue")
	    if (PLOT.LINEAR)  points(LuZ.0m.linear[i],depth.0, pch = 1, cex=1.8, col = "blue")
	    if (PLOT.LINEAR)  points(aop[ix.Z.interval[i],i], Z.interval[i], cex=1.8,
	           pch = 19,col ="blue")
	    abline(v=LuZ.detect.lim[i], col="orange", lwd=3)
	    abline(h=sub.surface.removed.layer.optics["LuZ"], col="green", lwd=2)
	    if (i == floor(seq(ix.w[1], ix.w[length(ix.w)], length.out = 4))[4]) legend("bottomright", c("LOESS", "Linear"),
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
	        xlab = expression(K[z]~"("*L[u]*"z) (local K, i.e. derivative of Lu(z))"),
	        ylab = "Depth (m)",col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(K0.fitted, depth.fitted[-1], type = "l",
	        lty = 1, ylim = rev(range(Depth)),
	        xlab = expression(K[0]~"("*L[u]*"z) (Depth-integrated from 0- to Z"),
	        ylab = "Depth (m)", col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	plot(temperature, temperature.depth, type = "p", pch=19, cex=0.8,
	        lty = 1, ylim = rev(range(temperature.depth)),
	        xlab = "Temperature (Degree)",
	        ylab = "Depth (m)",
	     main="Water Temperature profile from LuZ sensor")
	lines(temperature.fitted, depth.fitted, col = 2, lwd=2)

	if (!PLOT.LINEAR) Z.interval <-NA

	return(list(
		LuZ.fitted = aop.fitted,
		KZ.LuZ.fitted = KZ.fitted,
		K0.LuZ.fitted = K0.fitted,
		LuZ.0m = aop.0,
		K.LuZ.surf = K.surf,
		LuZ.Z.interval = Z.interval,
		LuZ.ix.Z.interval = ix.Z.interval,
		LuZ.0m.linear = LuZ.0m.linear,
		LuZ.linear.r2 = r2,
		LuZ.KolmolSmirnov.p.value = KolmolSmirnov.p.value,
		LuZ.detection.limit = LuZ.detect.lim,
		LuZ.temperature.fitted = temperature.fitted
	))
}
