
compute.bottom <- function(cops, distance.above.bottom.file.cut=0.15) {

  # Compute the bottom depth assuming the file was cleaned
  # using the CLEAN.FILES=TRUE option of cops.go() and
  # the user click when the COPS touch the bottom
  #
  bottom.depth <- max(cops$Depth)+delta.capteur.optics[depth.is.on]+distance.above.bottom.file.cut
  ix.depth.over.bottom <- which.min(abs(cops$depth.fitted - (bottom.depth-0.30) ))
  depth.over.bottom =  bottom.depth - cops$depth.fitted[ix.depth.over.bottom]

  if("LuZ" %in% instruments.optics & !("EuZ" %in% instruments.optics)  ) {
    Rb.Q <- NULL
    R.Z.Luz <- cops$LuZ.fitted * pi / cops$EdZ.fitted
    Rb.LuZ <- R.Z.Luz[ix.depth.over.bottom,]
    Rb.EuZ <- NULL
    if (all(is.na(Rb.LuZ))) Rb.LuZ <- NULL
  }
  if("EuZ" %in% instruments.optics & !("LuZ" %in% instruments.optics)) {
    Rb.Q <- NULL
    Rb.LuZ <- NULL
    R.Z.Euz <- cops$EuZ.fitted / cops$EdZ.fitted
    Rb.EuZ <- R.Z.Euz[ix.depth.over.bottom,]
    if (all(is.na(Rb.EuZ))) Rb.EuZ <- NULL
  }

  if("EuZ" %in% instruments.optics & "LuZ" %in% instruments.optics) {
    Q <- cops$EuZ.fitted/cops$LuZ.fitted
    Rb.Q <- Q[ix.depth.over.bottom,]
    R.Z.Luz <- cops$LuZ.fitted * pi / cops$EdZ.fitted
    Rb.LuZ <- R.Z.Luz[ix.depth.over.bottom,]
    if (all(is.na(Rb.LuZ))) Rb.LuZ <- NULL
    R.Z.Euz <- cops$EuZ.fitted / cops$EdZ.fitted
    Rb.EuZ <- R.Z.Euz[ix.depth.over.bottom,]
    if (all(is.na(Rb.EuZ))) Rb.EuZ <- NULL
  }

  if (is.null(Rb.LuZ) & is.null(Rb.EuZ)) {
    mymessage("WARNING: Bottom reflectance in NULL", head = "*")
    return(list(bottom.depth = bottom.depth,
                Rb.depth.over.bottom = depth.over.bottom,
                Rb.LuZ = Rb.LuZ,
                Rb.EuZ = Rb.EuZ,
                Rb.Q = Rb.Q))

  }


  # PLOT
  aop.cols <- rainbow.modified(length(cops$Ed0.waves))
  if("LuZ" %in% instruments.optics & !("EuZ" %in% instruments.optics)) {
    if(INTERACTIVE) x11(width = win.width, height = win.height)
    matplot(R.Z.Luz, cops$depth.fitted, type = "l", lty=1, #log = "x",
            ylim = rev(range(cops$depth.fitted)),
            xlim=c(0, min(c(0.5, max(Rb.LuZ,na.rm=T)))  ), #pch = ".", cex = 1,
            ylab="Depth (m)",
            xlab = expression(R[Lu]*z* ~"(unitless)"),
            col = aop.cols)
    grid(col = 1)
    if (!all(is.na(Rb.LuZ))) points(Rb.LuZ, rep(cops$depth.fitted[ix.depth.over.bottom], length(Rb.LuZ)),
           pch=19, cex=1.5, col = aop.cols)
    par(xpd = TRUE)
    legend(10^par("usr")[1], par("usr")[4], legend = cops$Ed0.waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(cops$Ed0.waves) / 2), cex = 0.75)
    par(xpd = FALSE)

    if (!all(is.na(Rb.LuZ))){
    if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.LuZ, type="l", lwd=3,
           ylim=c(0, min(c(0.5, max(Rb.LuZ,na.rm=T)))  ),
           main=paste("Bottom depth:", as.character(signif(bottom.depth,3)), "m"),
           xlab="Wavelength (nm)", ylab="Reflectance (unitless)", cex=1.5)
      legend("topleft", expression(pi*L[u]/E[d]), lwd=3, col=1, cex=1.8)
    }
  }

  if("EuZ" %in% instruments.optics & !("LuZ" %in% instruments.optics)) {
    if(INTERACTIVE) x11(width = win.width, height = win.height)
    matplot(R.Z.Euz, cops$depth.fitted, type = "l", lty=1, #log = "x",
            ylim = rev(range(cops$depth.fitted)),
            xlim=c(0, min(c(0.5, max(Rb.EuZ,na.rm=T)))  ), #pch = ".", cex = 1,
            ylab="Depth (m)",
            xlab = expression(R[Eu]*z* ~"(unitless)"),
            col = aop.cols)
    grid(col = 1)
    if (!all(is.na(Rb.EuZ))) points(Rb.EuZ, rep(cops$depth.fitted[ix.depth.over.bottom], length(Rb.EuZ)),
           pch=19, cex=1.5, col = aop.cols)
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
  }



  if("EuZ" %in% instruments.optics & ("LuZ" %in% instruments.optics)) {
    if(INTERACTIVE) x11(width = win.width, height = win.height)
    matplot(R.Z.Euz, cops$depth.fitted, type = "l", lty=1, #log = "x",
            ylim = rev(range(cops$depth.fitted)),
            xlim=c(0, min(c(0.5, max(Rb.EuZ,na.rm=T)))  ), #pch = ".", cex = 1,
            ylab="Depth (m)",
            xlab = expression(R*z* ~"(unitless)"),
            col = aop.cols)
    matplot(R.Z.Luz, cops$depth.fitted, type = "l", lty=2,
            add = TRUE,
            col = aop.cols)
    grid(col = 1)
    if (!all(is.na(Rb.EuZ))) points(Rb.EuZ, rep(cops$depth.fitted[ix.depth.over.bottom], length(Rb.EuZ)),
           pch=19, cex=1.2, col = aop.cols)
    if (!all(is.na(Rb.LuZ))) points(Rb.LuZ, rep(cops$depth.fitted[ix.depth.over.bottom], length(Rb.LuZ)),
           pch=1, cex=1.5, col = aop.cols)
    par(xpd = TRUE)
    legend(10^par("usr")[1], par("usr")[4], legend = cops$Ed0.waves, xjust = 0, yjust = 0, lty = 1, lwd = 2, col = aop.cols, ncol = ceiling(length(cops$Ed0.waves) / 2), cex = 0.75)
    par(xpd = FALSE)

    if (!all(is.na(Rb.EuZ))) {
      if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.EuZ, type="l", lwd=3,
           ylim=c(0, min(c(0.5, max(Rb.EuZ,na.rm=T)))  ),
           main=paste("Bottom depth:", as.character(signif(bottom.depth,3)), "m"),
           xlab="Wavelength (nm)", ylab="Reflectance (unitless)", cex=1.5)
      lines(cops$Ed0.waves, Rb.LuZ, lwd=3, col=2)
      legend("topleft", c(expression(E[u]/E[d]) , expression(pi*L[u]/E[d])), lwd=c(3,3), col=c(1,2), cex=1.8)
    }

    if (!all(is.na(Rb.Q))) {
      if(INTERACTIVE) x11(width = win.width, height = win.height)
      plot(cops$Ed0.waves, Rb.Q, type="l", lwd=3,
           #ylim=c(0, min(c(0.5, max(Rb.EuZ,na.rm=T)))  ),
           main="Bottom BRDF",
           xlab="Wavelength (nm)", ylab=expression("Q-factor (",sr^-1,")"), cex=1.5)
    }

  }

  return(list(bottom.depth = bottom.depth,
              Rb.depth.over.bottom = depth.over.bottom,
              Rb.LuZ = Rb.LuZ,
              Rb.EuZ = Rb.EuZ,
              Rb.Q = Rb.Q))

}
