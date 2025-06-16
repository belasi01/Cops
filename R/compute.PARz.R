#'   Compute PAR at depth from surface irradiance (Ed0+) and profile of
#'   downwelling irradiance (EdZ)
#'
#'@param Depth is a vector of depth corresponding to the EdZ measurements
#'@param waves is a vector of wavelength of EdZ and Ed0+
#'@param Edz is a matrix of EdZ (col=waves; row=Depth) after nomalization to Ed.0 variation
#'@param Ed.0 is a is a vector surface irradiance
#'@param f.PAR is a vector of numeric values corresponting to light fraction.
#'   The depth (in m) of each light fraction will be computed.
#'   Default is f.PAR=c(0.001, 0.01, 0.1,0.5)
#'@param z.fixed  is a vector of numeric values corresponting to depth at which light fraction will be calculated.
#'   Default is z.fixed=c(5,10,20,30,40)
#'
#'@return The program will return three vectors for
#'  1) the depth of each PAR fraction requested (z.f.PAR);
#'  2) the fraction of PAR at the depth requested (PAR.at.z);
#'  3) the vector of PAR at all depth Z (PAR.z) in micro mol photon / m^2
#'
#'@author Simon Belanger
#'@export
compute.PARz <- function(Depth, waves, Edz, Ed.0,
                         f.PAR=c(0.001, 0.01, 0.1,0.5),
                         z.fixed=c(5,10,20,30,40)) {


  # Convert  microW/cm^-2.nm to Quanta/m-2.nm.s
  c = 2.9979e17 # in nm/sec
  h = 6.6255e-34 # in J sec
  Q0 = Ed.0 * waves / (h*c) * 0.01 # factor -0.01 to convert to W/m^-2.nm

  # integrated
  Q0[is.na(Q0)] <- 0
  fx.linear <- approxfun(waves, Q0)
  PAR.0m = integrate(fx.linear, 380, 700, subdivisions=350, stop.on.error = FALSE)[1]
  PAR.0m = PAR.0m$value


  nz = length(Depth)
  PAR.z = rep(NA,nz)
  for (i in 1:nz){
      QZ = Edz[i,] * waves / (h*c) * 10000/1e6 #
      # integrated
      QZ[is.na(QZ)] <- 0
      if (any(is.infinite(QZ))) {
        print('WARNING:  infinite values in EdZ vector for PAR calculation')
        PAR.z[i] = 0
      } else {
        fx.linear <- approxfun(waves, QZ)
        tmp = integrate(fx.linear, 380, 700, subdivisions=350, stop.on.error = FALSE)[1]
        PAR.z[i] = tmp$value
      }
  }

  # Compute the depth of 90%, 50%, 30%, 10%, 1% and 0.1% PAR
  res= spline((PAR.z/PAR.0m), Depth, xout=f.PAR)
  z.f.PAR= cbind(f.PAR*100,res$y)
  z.f.PAR = as.data.frame(z.f.PAR[length(f.PAR):1,])
  names(z.f.PAR) = c("%PAR", "z")

  # compute %PAR at fixed depth

  res= spline(Depth, (PAR.z/PAR.0m), xout=z.fixed[z.fixed < max(Depth)])
  PAR.at.z= as.data.frame(cbind(z.fixed[z.fixed < max(Depth)],res$y*100))
  names(PAR.at.z) = c("z.fixed", "%PAR")


  return(list(z.f.PAR=z.f.PAR,
              PAR.z=PAR.z / 6.06E23 *1e6,
              PAR.at.z=PAR.at.z))
}
