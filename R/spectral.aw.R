#'
#'@title Pure water absorption spectrum
#'
#'
#'@description Compute spectral absorption coefficient for pure water from 280 to 900 nm
#'from Pope and Fry 1997 and Buiteveld 1994 experiments.
#'Optionnaly, K_w from Morel 1988 minus b_bw can be requested
#'
#' @param waves is the wavelenght (could be a vector)
#' @param MOREL is a logical to compute pure water absorption from Morel 1988 table.
#'
#'@author Simon Bélanger
#'
#'@export
spectral.aw  <- function(waves, MOREL=FALSE)
{
  
  if (MOREL) {
    Kw <- spline(KDTable_MM01$V1, KDTable_MM01$V2,  xout=waves, method="natural")
    aw <-  Kw$y - spectral.bw(waves)*0.5
    return(aw)
  } else {
    aw <- spline(AWTable$V1, AWTable$V2,  xout=waves, method="natural")$y
    return(aw)
  }
  
}