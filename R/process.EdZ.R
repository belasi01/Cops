process.EdZ <- function(cops.raw,
                        cops.dd,
                        cops.black,
                        cops.Ed0,
                        EXTRAPOLATION.0m = TRUE) {

	mymessage(paste("Processing EdZ", " ..."), head = "+")

	aop <- cops.raw$EdZ
	correction <- cops.Ed0$Ed0.correction
	aop <- aop * correction
	waves <- as.numeric(cops.raw$EdZ.waves)
	Depth <- cops.dd$Depth + delta.capteur.optics["EdZ"]
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

# tilt limitation
	valid.tilt <- tilt < tiltmax.optics["EdZ"]
	aop.all <- aop[cops.dd$Depth.good & valid.tilt,] # make a backup
	Depth.all <- Depth[cops.dd$Depth.good & valid.tilt]
	Depth.kept <- cops.dd$Depth.good & valid.tilt & Depth > sub.surface.removed.layer.optics["EdZ"]
	Depth <- Depth[Depth.kept]
	tilt <- tilt[Depth.kept]
	aop <- aop[Depth.kept, ]

	##### Remove radiometric measurement below the detection limit
	aop.all[aop.all < 0] <-0
	for (w in 1:length(waves)) {
	  aop[aop[,w] <= EdZ.detect.lim[w],w] <- NA
	}

# METHOD loess to fit this aop
  if (EXTRAPOLATION.0m) {
    idx.depth.0 <- which.min(abs(depth.fitted - 0))
    #  idx.depth.0 <- 1  ### Il me semble que ca donne la même chose...
  } else {
    idx.depth.0 <- which.min(abs(depth.fitted - Depth[1])) # ice version
  }
	depth.0 <- depth.fitted[idx.depth.0]
	span <- depth.interval.for.smoothing.optics["EdZ"]
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
	for (w in 1:length(waves)) {
	  print(paste("Clean AOP for EdZ ", waves[w]))
	  if (!all(is.na(aop.fitted[,w]))) { # if all NA, then the AOPs allready equal to NA
	    # Apply a spline on raw data for further flaging on the AOP
	    tmp = smooth.spline(Depth.all, aop.all[,w], spar=0.2) # spar was added to make the data smoother
	    aop.spline = spline(tmp, xout = depth.fitted, method = 'natural')$y
	    # remove bad data
	    KZ.fitted[(aop.fitted[(2:n.fitted),w] <= EdZ.detect.lim[w] |
	                 aop.spline[2:n.fitted]     <= EdZ.detect.lim[w] &
	                 depth.fitted[2:n.fitted] > Depth[1]),w] <- NA
	    K0.fitted[(aop.fitted[(2:n.fitted),w] <= EdZ.detect.lim[w] |
	                 aop.spline[2:n.fitted]   <= EdZ.detect.lim[w]&
	                 depth.fitted[2:n.fitted] > Depth[1]),w] <- NA
	    aop.fitted[(aop.fitted[,w]            <= EdZ.detect.lim[w] |
	                  aop.spline               <= EdZ.detect.lim[w]&
	                  depth.fitted > Depth[1]),w] <- NA
	  }
	  if (!is.na(aop.0[w])) {
	    if (aop.0[w] <= EdZ.detect.lim[w]) aop.0[w] <- NA
	  }
	}

	# Extrapolate EdZ to 0- using linear method
	K <- compute.Ksurf.linear(Depth, aop,
	                          instrument = "EdZ",
	                          delta.depth= linear.fit.max.delta.depth.optics["EdZ"],
	                          r2.threshold=linear.fit.Rsquared.threshold.optics["EdZ"])
	r2 <- K$r2
	K.surf <- K$Kx
	Z.interval <- K$Z.interval
	ix.Z.interval <- K$ix.Z.interval
	if (EXTRAPOLATION.0m) {
	  EdZ.0m.linear <- K$X.0m
	} else {
	  EdZ.0m.linear <- K$X.0m * exp(-depth.0*K.surf)
	}
	KolmolSmirnov.p.value<-K$KolmolSmirnov.p.value
	EdZ.fitted <- matrix(NA, ncol=dim(aop.fitted)[2], nrow=dim(aop.fitted)[1])
	for (i in 1:length(waves)) {
	  if (!is.na(EdZ.0m.linear[i])) {
	    EdZ.fitted[idx.depth.0:n.fitted,i] <- K$X.0m[i] * exp(-depth.fitted[idx.depth.0:n.fitted]*K.surf[i])
	  }
	}

	# Compute PAR at depth
	PAR <- compute.PARz(depth.fitted, waves, aop.fitted, aop.0,
	                    f.PAR=c(0.001, 0.01, 0.05, 0.1,0.5))

	PLOT.LINEAR <- !all(is.na(EdZ.0m.linear))
	if (!PLOT.LINEAR) Z.interval=5

# PLOT
	aop.cols <- rainbow.modified(length(waves))
	if(INTERACTIVE) x11(width = win.width, height = win.height)
  matplot(aop.all, Depth.all, type = "p", log = "x",
          ylim = c(max(Z.interval,na.rm = T)+1,0),
          #ylim = rev(range(c(0, Depth))),
          xlim=c(min(EdZ.detect.lim),max(aop, na.rm=T)), pch = ".", cex = 1,
          ylab="Depth (m)",
          xlab = expression(E[d]*z* ~~ "("*mu*W.*cm^{-2}*.nm^{-1}*")"),
          col = aop.cols)
  grid(col = 1)
	matplot(aop.fitted, depth.fitted, type = "l",
	        lty = 1, lwd = 2, add = TRUE, col = aop.cols)
	points(aop.0, rep(depth.0, length(aop.0)), col = aop.cols)
	if (PLOT.LINEAR) matplot(EdZ.fitted, depth.fitted, type = "l",
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
	         xlim = range(aop[aop[, i] > 0, i], aop.0[i], na.rm = TRUE),
	         ylim = rev(range(Depth, depth.fitted)),
	         pch = ".",
	         xlab = "", ylab = "", axes = FALSE, frame.plot = TRUE,
	         main = substitute(E[d]*z~x~"("*mu*W.*cm^{-2}*.nm^{-1}*")",list(x = waves[i])))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = 2)
	    points(aop.0[i], depth.0, pch = 20, col = 4)
	    abline(v=EdZ.detect.lim[i], col="orange", lwd=3)
	    abline(h=sub.surface.removed.layer.optics["EdZ"], col="green", lwd=3)
	    axis(1)
	    axis(2)
	  }
	}
	par(mai = mai.old2)
	par(mgp = mgp.old2)
	par(mfrow = c(1, 1))
	par(mai = mai.old1)
	par(mgp = mgp.old1)

	##### check surface extrapolation
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	mai.old1 <- par("mai")
	mgp.old1 <- par("mgp")
	par(mfrow = c(4, 5))
	par(mai = par("mai") / 2, mgp = par("mgp") / 2)
	mai.old2 <- par("mai")
	mgp.old2 <- par("mgp")
	for(i in 1:length(waves)) {
	  if (length(which(!is.na(aop[,i]))) > 0) {
	    if (PLOT.LINEAR) {
	      my.xlim = range(aop[, i], aop.0[i], EdZ.0m.linear[i], na.rm = TRUE)
	    } else {
	      my.xlim = range(aop[, i], aop.0[i], na.rm = TRUE)
	    }
	    plot(aop.all[, i], Depth.all, type = "p", log = "x",
	         xlim = my.xlim,
	         ylim = c(max(Z.interval,na.rm = T)+0.5,0),
	         pch = ".", xlab = "", ylab = "",
	         axes = FALSE, frame.plot = TRUE,
	         main = substitute(E[d]*z~x~r^2==r.2,list(x = waves[i], r.2=signif(r2[i],3))))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = "red")
	    points(aop.0[i], depth.0, pch = 1, cex=1.5, col = "red")
	    if (PLOT.LINEAR) lines(EdZ.fitted[, i], depth.fitted, col = "blue")
	    if (PLOT.LINEAR) points(EdZ.0m.linear[i],depth.0, pch = 1, cex=1.5, col = "blue")
	    if (PLOT.LINEAR) points(aop[ix.Z.interval[i],i], Z.interval[i], cex=1.5,
	           pch = 19,col ="blue")
	    abline(v=EdZ.detect.lim[i], col="orange", lwd=2)
	    abline(h=sub.surface.removed.layer.optics["EdZ"], col="green", lwd=1)
	    axis(1)
	    axis(2)
	  }
	}
	par(mai = mai.old2)
	par(mgp = mgp.old2)
	par(mfrow = c(1, 1))
	par(mai = mai.old1)
	par(mgp = mgp.old1)

	##### Same as above but 4 selected wavelenghts
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
	    if (PLOT.LINEAR) {
	      my.xlim = range(aop[, i], aop.0[i], EdZ.0m.linear[i], na.rm = TRUE)
	    } else {
	      my.xlim = range(aop[, i], aop.0[i], na.rm = TRUE)
	    }
	    plot(aop.all[, i], Depth.all, type = "p", log = "x",
	         xlim = my.xlim,
	         ylim = c(max(Z.interval,na.rm = T)+0.5,0),
	         pch = 19, xlab = "", ylab = "",
	         axes = FALSE, frame.plot = TRUE,
	         main = substitute(E[d]*z~x~"("*mu*W.*cm^{-2}*.nm^{-1}*")"~r^2==r.2,list(x = waves[i], r.2=signif(r2[i],3))))
	    grid(col = 1)
	    lines(aop.fitted[, i], depth.fitted, col = "red")
	    points(aop.0[i], depth.0, pch = 1, cex=1.5, col = "red")
	    if (PLOT.LINEAR) lines(EdZ.fitted[, i], depth.fitted, col = "blue")
	    if (PLOT.LINEAR) points(EdZ.0m.linear[i],depth.0, pch = 1, cex=1.8, col = "blue")
	    if (PLOT.LINEAR) points(aop[ix.Z.interval[i],i], Z.interval[i], cex=1.8,
	           pch = 19,col ="blue")
	    abline(v=EdZ.detect.lim[i], col="orange", lwd=3)
	    abline(h=sub.surface.removed.layer.optics["EdZ"], col="green", lwd=2)
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
	        xlab = expression(K[z]~"("*E[d]*"z) (local K, i.e. derivative of Ed(z))"),
	        ylab = "Depth (m)",col = aop.cols)
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

	if(INTERACTIVE) x11(width = win.width, height = win.height)
	if (any(!is.na(K0.fitted))){
	  matplot(K0.fitted, depth.fitted[-1], type = "l",
	          lty = 1, ylim = rev(range(Depth)),
	          xlab = expression(K[0]~"("*E[d]*"z) (Depth-integrated from 0- to Z"),
	          ylab = "Depth (m)", col = aop.cols)

	}
	grid(col = 1)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

  if (EXTRAPOLATION.0m) {
    #Scatterplot
    #############################
    par(mfrow = c(2, 1))
    Ed0.0p.m = cops.Ed0$Ed0.0p*0.957
    plot(Ed0.0p.m, aop.0,
         xlab = "Calculated Ed(0-) from Ed(0+)",
         ylab = "Extrapolated Ed(0-) from EdZ",
         xlim=c(0,max(aop.0+30, na.rm=T)),
         ylim=c(0,max(aop.0+30, na.rm=T)),
         pch=19,col="black",
         main =cops.raw$file)
    if (PLOT.LINEAR) points(Ed0.0p.m, EdZ.0m.linear,pch=19,col="blue")
    lines(c(0,200), c(0,200), col=2)
    legend("bottomright",legend=c("Ed(0-)linear","Ed(0-)LOESS"),
           text.col=c("blue", "black"),pch=c(19,19),
           col=c("blue", "black"))

    #Ed(0-)Ratio
    #############################

    Ed0.ratio.linear = EdZ.0m.linear/Ed0.0p.m
    Ed0.ratio = aop.0/Ed0.0p.m
    plot(cops.raw$Ed0.waves,Ed0.ratio,
         xlab = "Wavelength(nm)",
         ylab = "Ratio of extrapolated to calculated Ed(0-)",
         pch=19,col="black",
         ylim= c(0.6,1.4))
    abline(h=1.0, col="black")
    abline(h=c(0.9,1.1), col="black")
    abline(h=c(0.95,1.05), col="red")
    if (PLOT.LINEAR) points(cops.raw$Ed0.waves,Ed0.ratio.linear,
           xlab = NA, ylab = NA,pch=19,col="blue")
    legend("topright",c("linear","LOESS"),
           text.col=c("blue", "black"),
           pch=c(19,19),
           col=c("blue", "black"))

    ### Plot PAR
    ### The last depth is remove be cause it is often bad
    par(mfrow = c(1, 2))
    plot(PAR$PAR.z[1:(n.fitted-1)], depth.fitted[1:(n.fitted-1)],
         type = "l", lwd=2,
         lty = 1, ylim = rev(range(depth.fitted)), log="x",
         xlab = expression(PAR~"("*mu*mol*.photon*.m^{-2}*.s^{-1}*")"),
         ylab = "Depth (m)", main = "Downwelling Photosynthetic Active Radiation")
    plot(PAR$PAR.z[1:(n.fitted-1)]/PAR$PAR.z[1]*100, depth.fitted[1:(n.fitted-1)],
         type = "l", lwd=2,
         lty = 1, ylim = rev(range(depth.fitted)),
         xlab = expression(PAR~"(%)"), log="x",
         ylab = "Depth (m)")
    for (i in 1:5){
      if (PAR$z.f.PAR[i,2]<max(depth.fitted)) {
        points(PAR$z.f.PAR[i,], pch=19, col=2, cex=1.5)
        posi=4
        if (i == 1) posi = 2
        text(PAR$z.f.PAR[i,], paste(PAR$z.f.PAR[i,1],
                                    "% PAR at z=",
                                    signif(PAR$z.f.PAR[i,2],3)),
             pos=posi)

      }
    }
    par(mfrow = c(1, 1))

  }

	if (!PLOT.LINEAR) Z.interval <-NA
	return(list(
		EdZ.fitted = aop.fitted,
		KZ.EdZ.fitted = KZ.fitted,
		K0.EdZ.fitted = K0.fitted,
		EdZ.0m = aop.0,
		K.EdZ.surf = K.surf,
		EdZ.Z.interval = Z.interval,
		EdZ.ix.Z.interval = ix.Z.interval,
		EdZ.0m.linear = EdZ.0m.linear,
		EdZ.linear.r2 = r2,
		EdZ.KolmolSmirnov.p.value = KolmolSmirnov.p.value,
		EdZ.detection.limit = EdZ.detect.lim,
		PARd.z         = PAR$PAR.z,
		PARd.at.z      = PAR$PAR.at.z
	))
}
