
compute.bottom <- function(cops, distance.above.bottom.file.cut=0.15) {

  # Compute the bottom depth assuming the file was cleaned
  # using the CLEAN.FILES=TRUE option of cops.go() and
  # the user click when the COPS touch the bottom
  #
  bottom.depth <- max(cops$Depth)+delta.capteur.optics[depth.is.on]+distance.above.bottom.file.cut
  ix.depth.over.bottom <- which.min(abs(cops$depth.fitted - (bottom.depth-0.30) ))
  depth.over.bottom <-  bottom.depth - cops$depth.fitted[ix.depth.over.bottom]
  depth.fitted <- c(cops$depth.fitted, as.numeric(bottom.depth)) # add the bottom depth to the depth.fitted vector for extrapolation to the bottom
  ix.bottom <- length(depth.fitted)

  if("LuZ" %in% instruments.optics & !("EuZ" %in% instruments.optics)  ) {
    Rb.Q <- NULL
    R.Z.Luz <- cops$LuZ.fitted * pi / cops$EdZ.fitted
    ### Extrapolate R.Z.LuZ to the bottom
    fitted <- fit.with.loess(cops$LuZ.waves,
                             cops$depth.fitted,
                             R.Z.Luz ,
                             cops$depth.interval.for.smoothing.optics["LuZ"],
                             depth.fitted, # fit on this new vector to reach the bottom.
                             idx.depth.0=1,
                             idx.bottom=ix.bottom,# will return the interpolated bottom reflectance
                             span.wave.correction = FALSE,
                             DEPTH.SPAN = TRUE,
                             minimum.obs = 10)
    R.Z.Luz <- fitted$aop.fitted
    Rb.LuZ.H0 <-  fitted$aop.bottom
    Rb.LuZ <- R.Z.Luz[ix.depth.over.bottom,]
    Rb.EuZ <- NULL
    if (all(is.na(Rb.LuZ))) Rb.LuZ <- NULL
    if (all(is.na(Rb.LuZ.H0))) Rb.LuZ.H0 <- NULL
  }
  if("EuZ" %in% instruments.optics & !("LuZ" %in% instruments.optics)) {
    Rb.Q <- NULL
    Rb.LuZ <- NULL
    R.Z.Euz <- cops$EuZ.fitted / cops$EdZ.fitted
    ### Extrapolate R.Z.EuZ to the bottom
    fitted <- fit.with.loess(cops$EuZ.waves,
                             cops$depth.fitted,
                             R.Z.Euz ,
                             cops$depth.interval.for.smoothing.optics["EuZ"],
                             depth.fitted, # fit on this new vector to reach the bottom.
                             idx.depth.0=1,
                             idx.bottom=ix.bottom,# will return the interpolated bottom reflectance
                             span.wave.correction = FALSE,
                             DEPTH.SPAN = TRUE,
                             minimum.obs = 10)
    R.Z.Euz <- fitted$aop.fitted
    Rb.EuZ.H0 <-  fitted$aop.bottom
    Rb.EuZ <- R.Z.Euz[ix.depth.over.bottom,]
    if (all(is.na(Rb.EuZ))) Rb.EuZ <- NULL
    if (all(is.na(Rb.EuZ.H0))) Rb.EuZ.H0 <- NULL
  }

  if("EuZ" %in% instruments.optics & "LuZ" %in% instruments.optics) {
    Q <- cops$EuZ.fitted/cops$LuZ.fitted
    ### Extrapolate Q-factor to the bottom
    fitted <- fit.with.loess(cops$LuZ.waves,
                             cops$depth.fitted,
                             Q,
                             cops$depth.interval.for.smoothing.optics["LuZ"],
                             depth.fitted, # fit on this new vector to reach the bottom.
                             idx.depth.0=1,
                             idx.bottom=ix.bottom,# will return the interpolated bottom reflectance
                             span.wave.correction = FALSE,
                             DEPTH.SPAN = TRUE,
                             minimum.obs = 10)
    Q <- fitted$aop.fitted
    Rb.Q.H0 <-  fitted$aop.bottom
    Rb.Q <- Q[ix.depth.over.bottom,]

    R.Z.Luz <- cops$LuZ.fitted * pi / cops$EdZ.fitted
    ### Extrapolate R.Z.LuZ to the bottom
    fitted <- fit.with.loess(cops$LuZ.waves,
                             cops$depth.fitted,
                             R.Z.Luz ,
                             cops$depth.interval.for.smoothing.optics["LuZ"],
                             depth.fitted, # fit on this new vector to reach the bottom.
                             idx.depth.0=1,
                             idx.bottom=ix.bottom,# will return the interpolated bottom reflectance
                             span.wave.correction = FALSE,
                             DEPTH.SPAN = TRUE,
                             minimum.obs = 10)
    R.Z.Luz <- fitted$aop.fitted
    Rb.LuZ.H0 <-  fitted$aop.bottom
    Rb.LuZ <- R.Z.Luz[ix.depth.over.bottom,]
    if (all(is.na(Rb.LuZ))) Rb.LuZ <- NULL
    if (all(is.na(Rb.LuZ.H0))) Rb.LuZ.H0 <- NULL


    R.Z.Euz <- cops$EuZ.fitted / cops$EdZ.fitted
    ### Extrapolate R.Z.EuZ to the bottom
    fitted <- fit.with.loess(cops$EuZ.waves,
                             cops$depth.fitted,
                             R.Z.Euz ,
                             cops$depth.interval.for.smoothing.optics["EuZ"],
                             depth.fitted, # fit on this new vector to reach the bottom.
                             idx.depth.0=1,
                             idx.bottom=ix.bottom,# will return the interpolated bottom reflectance
                             span.wave.correction = FALSE,
                             DEPTH.SPAN = TRUE,
                             minimum.obs = 10)
    R.Z.Euz <- fitted$aop.fitted
    Rb.EuZ.H0 <-  fitted$aop.bottom
    Rb.EuZ <- R.Z.Euz[ix.depth.over.bottom,]
    if (all(is.na(Rb.EuZ))) Rb.EuZ <- NULL
    if (all(is.na(Rb.EuZ.H0))) Rb.EuZ.H0 <- NULL
  }

  if (is.null(Rb.LuZ) & is.null(Rb.EuZ)) {
    mymessage("WARNING: Bottom reflectance in NULL", head = "*")
    return(list(bottom.depth = bottom.depth,
                Rb.depth.over.bottom = depth.over.bottom,
                Rb.LuZ = Rb.LuZ,
                Rb.EuZ = Rb.EuZ,
                Rb.Q = Rb.Q,
                Rb.LuZ.H0 = Rb.LuZ.H0, # Extrapolated value to the bottom
                Rb.EuZ.H0 = Rb.EuZ.H0,
                Rb.Q.H0 = Rb.Q.H0))

  }


  cops.shallow <-  list(bottom.depth = bottom.depth,
                        Rb.depth.over.bottom = depth.over.bottom,
                        Rb.LuZ = Rb.LuZ,
                        Rb.EuZ = Rb.EuZ,
                        Rb.Q = Rb.Q,
                        Rb.LuZ.H0 = Rb.LuZ.H0, # Extrapolated value to the bottom
                        Rb.EuZ.H0 = Rb.EuZ.H0,
                        Rb.Q.H0 = Rb.Q.H0)


  # PLOT
  aop.cols <- rainbow.modified(length(cops$Ed0.waves))
  # If only LuZ available
  if("LuZ" %in% instruments.optics & !("EuZ" %in% instruments.optics)) {
    if(INTERACTIVE) x11(width = win.width, height = win.height)
    matplot(R.Z.Luz, depth.fitted, type = "l", lty=1, #log = "x",
            ylim = rev(range(depth.fitted)),
            xlim=c(0, min(c(0.5, max(Rb.LuZ.H0,na.rm=T)))  ), #pch = ".", cex = 1,
            ylab="Depth (m)",
            xlab = expression(R[Lu]*z* ~"(unitless)"),
            col = aop.cols)
    grid(col = 1)
    if (!all(is.na(Rb.LuZ))) points(Rb.LuZ, rep(depth.fitted[ix.depth.over.bottom], length(Rb.LuZ)),
           pch=19, cex=1.5, col = aop.cols)
    if (!all(is.na(Rb.LuZ.H0))) points(Rb.LuZ.H0, rep(depth.fitted[ix.bottom], length(Rb.LuZ.H0)),
                                    pch=17, cex=1.5, col = aop.cols)
    par(xpd = TRUE)
    legend(10^par("usr")[1], par("usr")[4], legend = cops$Ed0.waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(cops$Ed0.waves) / 2), cex = 0.75)
    par(xpd = FALSE)

    if (!all(is.na(Rb.LuZ))){
    if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.LuZ, type="l", lwd=3,
           ylim=c(0, min(c(0.5, max(Rb.LuZ.H0,na.rm=T)))  ),
           main=paste("Bottom depth:", as.character(signif(bottom.depth,3)), "m"),
           xlab="Wavelength (nm)", ylab="Reflectance (unitless)", cex=1.5)
      points(cops$Ed0.waves, Rb.LuZ.H0, pch=17, cex=1.5, col = aop.cols)
      legend("topleft", expression(pi*L[u]/E[d]), lwd=3, col=1, cex=1.8)
    }
  }

  # If only EuZ available
  if("EuZ" %in% instruments.optics & !("LuZ" %in% instruments.optics)) {
    if(INTERACTIVE) x11(width = win.width, height = win.height)
    matplot(R.Z.Euz, depth.fitted, type = "l", lty=1, #log = "x",
            ylim = rev(range(depth.fitted)),
            xlim=c(0, min(c(0.5, max(Rb.EuZ.H0,na.rm=T)))  ), #pch = ".", cex = 1,
            ylab="Depth (m)",
            xlab = expression(R[Eu]*z* ~"(unitless)"),
            col = aop.cols)
    grid(col = 1)
    if (!all(is.na(Rb.EuZ))) points(Rb.EuZ, rep(cops$depth.fitted[ix.depth.over.bottom], length(Rb.EuZ)),
           pch=19, cex=1.5, col = aop.cols)
    if (!all(is.na(Rb.EuZ.H0))) points(Rb.EuZ.H0, rep(depth.fitted[ix.bottom], length(Rb.EuZ.H0)),
                                       pch=17, cex=1.5, col = aop.cols)
    par(xpd = TRUE)
    legend(10^par("usr")[1], par("usr")[4], legend = cops$Ed0.waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(cops$Ed0.waves) / 2), cex = 0.75)
    par(xpd = FALSE)

    if (!all(is.na(Rb.EuZ))){
      if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.EuZ, type="l", lwd=3,
           ylim=c(0, min(c(0.5, max(Rb.EuZ,na.rm=T)))  ),
           main=paste("Bottom depth:", as.character(signif(bottom.depth,3)), "m"),
           xlab="Wavelength (nm)", ylab="Reflectance (unitless)", cex=1.5)
      legend("topleft", expression(E[u]/E[d]), lwd=3, col=1, cex=1.8)
    }

    ######
    plot.bottom.spectra(c(cops, cops.shallow))
    ######

  }

  # If both EuZ and LuZ available
  if("EuZ" %in% instruments.optics & ("LuZ" %in% instruments.optics)) {
    if(INTERACTIVE) x11(width = win.width, height = win.height)
    matplot(R.Z.Euz, depth.fitted, type = "l", lty=1, #log = "x",
            ylim = rev(range(depth.fitted)),
            xlim=c(0, min(c(0.5, max(Rb.EuZ.H0,na.rm=T)))  ), #pch = ".", cex = 1,
            ylab="Depth (m)",
            xlab = expression(R*z* ~"(unitless)"),
            col = aop.cols)
    matplot(R.Z.Luz, depth.fitted, type = "l", lty=2,
            add = TRUE,
            col = aop.cols)
    grid(col = 1)
    if (!all(is.na(Rb.EuZ))) points(Rb.EuZ, rep(depth.fitted[ix.depth.over.bottom], length(Rb.EuZ)),
           pch=19, cex=1.2, col = aop.cols)
    if (!all(is.na(Rb.LuZ))) points(Rb.LuZ, rep(depth.fitted[ix.depth.over.bottom], length(Rb.LuZ)),
           pch=1, cex=1.5, col = aop.cols)
    if (!all(is.na(Rb.EuZ.H0))) points(Rb.EuZ.H0, rep(depth.fitted[ix.bottom], length(Rb.EuZ.H0)),
                                    pch=17, cex=1.2, col = aop.cols)
    if (!all(is.na(Rb.LuZ.H0))) points(Rb.LuZ.H0, rep(depth.fitted[ix.bottom], length(Rb.LuZ.H0)),
                                    pch=18, cex=1.5, col = aop.cols)
    par(xpd = TRUE)
    legend(10^par("usr")[1], par("usr")[4], legend = cops$Ed0.waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(cops$Ed0.waves) / 2), cex = 0.75)
    par(xpd = FALSE)

    if (!all(is.na(Rb.EuZ))) {
      if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.EuZ, type="l", lwd=3,
           ylim=c(0, min(c(0.5, max(c(Rb.EuZ.H0,Rb.LuZ.H0),na.rm=T)))  ),
           main=paste("Bottom depth:", as.character(signif(bottom.depth,3)), "m"),
           xlab="Wavelength (nm)", ylab="Reflectance (unitless)", cex=1.5)
      lines(cops$Ed0.waves, Rb.LuZ, lwd=3, col=2)
      lines(cops$Ed0.waves, Rb.LuZ.H0, lwd=3, col=2, lty=2)
      lines(cops$Ed0.waves, Rb.EuZ.H0, lwd=3, lty=2)
      legend("topleft", c(expression(E[u]/E[d]) , expression(pi*L[u]/E[d]),expression(E[u]/E[d](H[0])) , expression(pi*L[u]/E[d](H[0]))),
             lwd=c(3,3,3,3), col=c(1,2,1,2), lty = c(1,1,2,2), cex=1.8)
    }

    if (!all(is.na(Rb.Q))) {
      if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.Q, type="l", lwd=3,
           main="Bottom BRDF",
           xlab="Wavelength (nm)",
           ylab=expression("Q-factor (",sr^-1,")"),
           ylim=c(0, min(c(10, max(c(Rb.Q, Rb.Q.H0),na.rm=T)))),
           cex=1.5)
      lines(cops$Ed0.waves, Rb.Q.H0, lwd=3, col=3)
    }

    ######
    plot.bottom.spectra(c(cops, cops.shallow))
    ######

  }

  return(cops.shallow)

}
