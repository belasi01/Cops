compute.PAR.fitted <- function(cops) {

  ####################
  mymessage("Computing PAR from fitted AOPs ...", head = "+", tail = "-")

  ####################
  # atm. AOPs
  waves.0 <- cops$Ed0.waves
  Ed0 <- cops$Ed0.fitted
  Ed0[is.na(Ed0)] <- 0
  #correction <- cops$Ed0.correction !!!! NOTE : fitted AOP are already corrected, as fitting is applied within process.Xy.R !!!!!

  ####################
  # down AOPs
  waves.d <- cops$EdZ.waves
  EdZ <- cops$EdZ.fitted
  EdZ[is.na(EdZ)] <- 0

  ####################
  # up   AOPs
  if("EuZ" %in% instruments.optics) {
    waves.u <- cops$EuZ.waves
    EuZ <- cops$EuZ.fitted
    EuZ[is.na(EuZ)] <- 0
  }
  if("LuZ" %in% instruments.optics) {
    waves.u <- cops$LuZ.waves
    LuZ <- cops$LuZ.fitted
    EuZ <- LuZ * cops$Q.sun.nadir
    EuZ[is.na(EuZ)] <- 0
  }

####################

#get the Depth
Depth <- cops$depth.fitted

# define Planck's constant, light velocity in vacuum and Avoragro's number, in SI units
h  <- 6.62607004E-34 # m2.kg.s-1, or J.s
c  <- 299792458      # m.s-1
av <- 6.022140857E23 # mol-1

Ed0 <- Ed0 * 1E-2 # convert EdZ to SI units, i.e. W.m-2.nm-1
waves.0 <- waves.d *1E-9 # convert lambdas to SI units, i.e. m
for (i in 1:length(Depth)){
  Ed0[i,] <- Ed0[i,] * waves.0 / (h * c) # convert EdZ from Watts to Quantas
}

EdZ <- EdZ * 1E-2 # convert EdZ to SI units, i.e. W.m-2.nm-1
waves.d <- waves.d *1E-9 # convert lambdas to SI units, i.e. m
for (i in 1:length(Depth)){
  EdZ[i,] <- EdZ[i,] * waves.d / (h * c) # convert EdZ from Watts to Quantas
}

EuZ <- EuZ * 1E-2 # convert EdZ to SI units, i.e. W.m-2.nm-1
waves.u <- waves.u *1E-9 # convert lambdas to SI units, i.e. m
for (i in 1:length(Depth)){
  EuZ[i,] <- EuZ[i,] * waves.u / (h * c) # convert EuZ from Watts to Quantas
}
PAR.0.fitted <- array(NA, dim = c(length(Depth)), dimnames = list(Depth))
PAR.d.fitted <- array(NA, dim = c(length(Depth)), dimnames = list(Depth))
PAR.u.fitted <- array(NA, dim = c(length(Depth)), dimnames = list(Depth))
waves.PAR <- seq(400, 700, by=1)/1E9
for (i in 1:length(Depth)) {
  if(all(!is.na(Ed0[i, ]))){
    spline.0 <- spline(waves.0, Ed0[i,], xout = waves.PAR)
    PAR.0.fitted[i] <- sum(spline.0$y) # note: unit = quanta.m2.s-1
  }
  if(all(!is.na(EdZ[i, ]))){
    spline.d <- spline(waves.d, EdZ[i,], xout = waves.PAR)
    PAR.d.fitted[i] <- sum(spline.d$y) # note: unit = quanta.m2.s-1
  }
  if(all(!is.na(EuZ[i, ]))){
    spline.u <- spline(waves.d, EuZ[i,], xout = waves.PAR)
    PAR.u.fitted[i] <- sum(spline.u$y) # note: unit = quanta.m2.s-1
  }
}

PAR.0.fitted <- PAR.0.fitted * 1E6/av # convert to uEinstein.m-2.s-1
PAR.d.fitted <- PAR.d.fitted * 1E6/av # convert to uEinstein.m-2.s-1
PAR.u.fitted <- PAR.u.fitted * 1E6/av # convert to uEinstein.m-2.s-1

PAR.0.fitted[PAR.0.fitted <=0 ] <- NA
PAR.d.fitted[PAR.d.fitted <=0 ] <- NA
PAR.u.fitted[PAR.u.fitted <=0 ] <- NA

# PLOT

if(INTERACTIVE) x11(width = win.width, height = win.height)
par(mfrow = c(2, 2))
# plot an empty plot if Eu instead of Lu, just to align PAR.d and PAR.u plots on bottom of the page
# otherwise plot PAR.0 and PAR.d at top of the page
if("EuZ" %in% instruments.optics) {plot(1, 1, type = "n", xlab = "", ylab = "", axes = FALSE, frame.plot = F)}

plot(1, 1, type = "n",ylim = rev(range(Depth)),
    #xlim = c((median(PAR.0.fitted, na.rm = T) - 15 * sd(PAR.0.fitted, na.rm = T)),(median(PAR.0.fitted, na.rm = T) + 15 * sd(PAR.0.fitted, na.rm = T))),
    xlim = range(PAR.0.fitted, na.rm=T),
    log = "x",
    xlab = expression(PAR[0]~fitted ~~ "("*mu*Ein.*m^{-2}*.s^{-1}*", log scale)"),
    ylab = "Depth (m)",
    axes = FALSE,
    frame.plot = TRUE)
axis(2)
axis(1)
lines(PAR.0.fitted, Depth, type = "b")
par(xpd = TRUE)
legend("topleft", legend = "PAR estimated from fitted atmospheric irradiance values", xjust = 0, yjust = 0, lty = 1, lwd = 2, cex = 0.75)
par(xpd = FALSE)

plot(1, 1, type = "n",
     #xlim = c(0,(median(PAR.d.fitted, na.rm = T) + 10 * sd(PAR.d.fitted, na.rm = T))),
     xlim = range(PAR.d.fitted, na.rm=T),
     log = "x",
     ylim = rev(range(Depth)),
     xlab = expression(PAR[d]~fitted ~~ "("*mu*Ein.*m^{-2}*.s^{-1}*", log scale)"),
     ylab = "Depth (m)", axes = FALSE,
     frame.plot = TRUE)
axis(2)
axis(1)
lines(PAR.d.fitted, Depth, type = "b")
par(xpd = TRUE)
legend("topleft", legend = "PAR estimated from fitted downward irradiance values", xjust = 0, yjust = 0, lty = 1, lwd = 2, cex = 0.75)
par(xpd = FALSE)

if("EuZ" %in% instruments.optics) {
  # plot(1, 1, type = "n", ylim = rev(range(Depth)), xlim = c(0,(median(PAR.u.fitted, na.rm = T) + 6 * sd(PAR.u.fitted, na.rm = T))),
  #      xlab = expression(PAR[u]~fitted ~~ "("*mu*Ein.*m^{-2}*.s^{-1}*")"), ylab = "Depth (m)", axes = FALSE, frame.plot = TRUE)
  plot(1, 1, type = "n",
       ylim = rev(range(Depth)),
       xlim = range(PAR.u.fitted, na.rm=T),
       log = "x",
       xlab = expression(PAR[u]~fitted ~~ "("*mu*Ein.*m^{-2}*.s^{-1}*", log scale)"),
       ylab = "Depth (m)",
       axes = FALSE,
       frame.plot = TRUE)
  axis(2)
  axis(1)
  lines(PAR.u.fitted, Depth, type = "b")
  par(xpd = TRUE)
  legend(par("usr")[1], par("usr")[4], legend = "PAR estimated from fitted upward irradiance values", xjust = 0, yjust = 0, lty = 1, lwd = 2, cex = 0.75)
  par(xpd = FALSE)
}

par(mfrow = c(1, 1))
if("EuZ" %in% instruments.optics) {
  return(c(list(PAR.0.fitted = PAR.0.fitted,
                PAR.d.fitted = PAR.d.fitted,
                PAR.u.fitted = PAR.u.fitted)))
} else {
  return(c(list(PAR.0.fitted = PAR.0.fitted,
                PAR.d.fitted = PAR.d.fitted)))
}
}
