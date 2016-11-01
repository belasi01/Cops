process.Ed0.BioShade <- function(cops.raw, cops.dd, cops.black) {

	mymessage(paste("Processing Ed0 for BioShade", " ..."), head = "+")

	Ed0.raw <- cops.raw$Ed0
	Ed0 <- Ed0.raw
	waves <- as.numeric(cops.raw$Ed0.waves)

  Time.raw <- (as.numeric(cops.dd$dates) + (as.numeric(cops.raw$Others$Millisecond)/1000)) - as.numeric(cops.dd$dates[1])
  Time <- Time.raw

  dates <- as.POSIXct(strptime(cops.raw$Others$DateTime, format = format.date))
  dates.secs.from.beginning <- as.numeric(dates) - min(as.numeric(dates))
  dates.good <- dates.secs.from.beginning >= time.window[1] & dates.secs.from.beginning <= time.window[2]


  BioShadePos.raw <- cops.raw$Others$BioShade_Position
  if (is.null(BioShadePos.raw)) BioShadePos.raw <- cops.raw$Others$BioShade.Position
  if (is.null(BioShadePos.raw)) BioShadePos.raw <- cops.raw$Others$`BioShade:Position`
  if (is.null(BioShadePos.raw)) {
    print("Could not find BioShade Position in the file")
    return(0)
  }
  BioShadePos <- BioShadePos.raw



	tilt <- cops.dd[["Ed0.tilt"]]
	sunzen <- cops.dd$sunzen
	month <- cops.dd$month
	day <- cops.dd$day
	if(!is.null(cops.black)) {
		black <- cops.black$Ed0
		Ed0 <- t(t(Ed0) - black)
	}

# tilt limitation
	valid.tilt <- tilt < tiltmax.optics["Ed0"]

# Agglomerate good records
  valid.rec <- valid.tilt & dates.good

  Ed0 <- Ed0[valid.rec, ]

#	Depth <- Depth[Depth.kept]
	tilt <- tilt[valid.rec]
  Time <- Time[valid.rec]
  BioShadePos <- BioShadePos[valid.rec]

# METHOD loess to fit this aop
  ix.no.shade = which(BioShadePos > 24000 | BioShadePos < 5000 )
  ix.shade = which(BioShadePos < 24000 | BioShadePos > 5000 )

  if(INTERACTIVE) x11(width = win.width, height = win.height)
  plot(Time, BioShadePos)
  points(Time[ix.no.shade], BioShadePos[ix.no.shade], col=2)


  fitted <- fit.with.loess(waves, Time[ix.no.shade], Ed0[ix.no.shade,], 0.8, Time, span.wave.correction = FALSE)
  Ed0.fitted <- fitted$aop.fitted

# theorical Ed0
	om.aer <-  0.98
	F.aer <- 0.75
	tau.a <- 0.10
	t <- exp(-(
		0.5 * tau.r(waves) +
		tau.oz(waves, du = 350) +
		(1 - om.aer * F.aer) * tau.a) / cos(sunzen *pi/180))
	Ed0.th <- cos(sunzen * pi / 180) * t * etirr(waves) * orbex(month,day)

	aop.cols <- rainbow.modified(length(waves))
	if(INTERACTIVE) x11(width = win.width, height = win.height)
	matplot(Time, Ed0, type = "p", xlim = range(Time), pch = ".", cex = 1, ylab = "Ed0", col = aop.cols)
	grid(col = 1)
  matplot(Time, Ed0.fitted, type = "l", lty = 1, lwd = 2, add = TRUE, col = aop.cols)
	par(xpd = TRUE)
	legend(par("usr")[1], par("usr")[4], legend = waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(waves) / 2), cex = 0.75)
	par(xpd = FALSE)

  # find the minimum Irradiance within the time range where
  # Bioshade was moving over the hemisphere.
  # Use 490 nm
  # This is equivalent ot point M in Fig. 55  of Morrow et al 2012 NASA Tech.
  ix.M = which.min(Ed0[ix.shade,9])
  ix.M = ix.shade[ix.M]
  points(Time[ix.M],Ed0[ix.M,9], cex=1.25, pch=19)
  text(Time[ix.M]+7,Ed0[ix.M,9], "M")

  # Here is a simplify method to get Edif values
  Ed0.tot = Ed0.fitted[ix.M,]
  Ed0.dif = Ed0[ix.M,]
  Ed0.f = Ed0.dif/Ed0.tot

  par(mar=c(5,4,4,5)+.1)
  plot(waves, Ed0.tot, type="l", lwd=2, xlab=expression(lambda), ylab="Ed0")
  lines(waves, Ed0.dif, type="l", lwd=2, col=4)
  lines(waves, Ed0.tot-Ed0.dif, type="l", lwd=2, col=2)

  par(new=TRUE)
  plot(waves, Ed0.f,,type="l",col=3,xaxt="n",yaxt="n",xlab="",ylab="")
  axis(4)
  mtext("f_dif",side=4,line=3)
  legend("bottomright", c("Ed0.tot", "Ed0.dif", "Ed0.dir", "Ed0.f"), col=c(1,4,2,3),lwd=c(2,2,2,2))

	return(list(
		Ed0.th = Ed0.th,
		Ed0.fitted = Ed0.fitted,
		Ed0.tot = Ed0.tot,
    Ed0.dif = Ed0.dif,
    Ed0.f = Ed0.f
	))
}
