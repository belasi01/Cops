
#

Rrs2FU <-function(Waves, Rrs){
  #CIE = read.table("~/Copy/R/Tables/CIE/lin2012xyz2e_1_7sf.csv", sep=",")
  #CIE = read.table("~/MEGA/R/Tables/CIE/ciexyz31.csv", sep=",")
  #names(CIE) <- c("waves", "x", "y", "z")

  # interpolate Rrs to CIE wavelength
  Rrs.int = spline(Waves, Rrs, xout=CIE$waves, method = "natural")$y

  Rrs.int[Rrs.int < 0] = 0
  # compute integrals
  fx.linear <- approxfun(CIE$waves, CIE$x*Rrs.int)
  X = integrate(fx.linear, 390, 830, subdivisions=340, stop.on.error = FALSE)[1]
  X = X$value

  fx.linear <- approxfun(CIE$waves, CIE$y*Rrs.int)
  Y = integrate(fx.linear, 390, 830, subdivisions=340, stop.on.error = FALSE)[1]
  Y = Y$value

  fx.linear <- approxfun(CIE$waves, CIE$z*Rrs.int)
  Z = integrate(fx.linear, 390, 830, subdivisions=340, stop.on.error = FALSE)[1]
  Z = Z$value

  sum = X+Y+Z

  x = X / sum
  y = Y / sum

  # compute the angle in the chromaticity space

  dx = x - 0.33
  dy = y - 0.33

  alpha.rad = atan(dy/dx)

  if (dx >= 0) {
    alpha.M = alpha.rad*180/pi
  } else alpha.M = 180 + alpha.rad*180/pi

  # Get the FU class

  if (alpha.M > alpha.T[1]) FU = 1
  if (alpha.M < alpha.T[20]) FU = 21

  for (i in 1:19) {
    if (alpha.M <= alpha.T[i] & alpha.M > alpha.T[i+1]) FU = i+1
  }



  return(list(x=x, y=y, FU=FU))
}


# From Table 5 in Novoa et al 2013
FU1= c(0.191, 0.167)
FU2= c(0.199 ,0.200 )
FU3= c(0.210, 0.240)
FU4= c(0.227 ,0.288 )
FU5= c(0.246 ,0.335 )
FU6= c( 0.266, 0.376)
FU7= c(0.291 ,0.412 )
FU8= c(0.315 ,0.440 )
FU9= c(0.337 ,0.462 )
FU10= c(0.363, 0.476 )
FU11= c(0.386, 0.487)
FU12= c(0.402, 0.481)
FU13= c(0.416, 0.474)
FU14= c(0.431, 0.466)
FU15= c(0.446, 0.458)
FU16= c(0.461, 0.449)
FU17= c(0.475, 0.441)
FU18= c(0.489, 0.433)
FU19= c(0.503, 0.425)
FU20= c(0.516, 0.416)
FU21= c(0.528, 0.408)

FU = rbind(FU1,FU2,FU3,FU4,FU5,FU6,FU7,FU8,FU9,FU10,
           FU11,FU12,FU13,FU14,FU15,FU16,FU17,FU18,FU19,FU20, FU21)

alpha = rep(NA,21)
for (i in 1:21) {
  dx = FU[i,1] - 0.33
  dy = FU[i,2] - 0.33

  alpha.rad = atan(dy/dx)

  if (dx >= 0) {
    alpha[i] = alpha.rad*180/pi
  } else alpha[i] = 180 + alpha.rad*180/pi

}
alpha.T = rep(NA,20)
for (i in 1:20) {
 alpha.T[i] = (alpha[i] + alpha[i+1]) / 2
}



