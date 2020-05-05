#' Extrapolate vertical profile of a radiometric
#' quantity (EdZ, EuZ, LuZ) to the air-sea interface using a linear
#' model applied to log-transformed data in an iterative scheme.
#'
#'
#'@param Depth is a vector of depth corresponding to the input quantity
#'@param aop is the matrix of EdZ, EuZ or LuZ. The matrix should be clean
#'after filtration for high instrument tilt.
#'@param instrument is the instrument type, "EdZ", "EuZ" or "LuZ"
#'@param r2.threshold is the minimum R^2 require to accept the fit
#'(Default is 0.80)
#'@param delta.depth is the depth interval toletared between the
#'first and the last depth condidered for the surface.
#'The Default is 2.5 meters. So if the very first measurement is
#'at 0.5 m, the "surface" layer is considered as 0.5 to 3 meters depth.
#'But this is the maximum layer considered.
#'The minimum layer is fixed at 0.30 m, so in this example it would be from
#'0.5 to 0.8 m.
#'
#'@details This function is called by
#'\code{\link{process.EdZ}}
#'\code{\link{process.EuZ}}
#'\code{\link{process.LuZ}}
#'
#'@return A list of list of 5 vectors of 19 values (one per wavelenght):
#'X.0m (is the quantity (EdZ, LuZ or EuZ) just below the sea surface),
#'Kx (is the diffuse attenuation coefficient of the surface layer considered),
#'Z.interval (is the maximum depth considered),
#'ix.Z.interval (is the index of the maximum depth considered),
#'r2 (is the R-squared of the regression),
#'detection.limit
#'
#'
#'
#'
#' @author Simon Bélanger
compute.Ksurf.linear <- function (Depth, aop,
                                  r2.threshold = 0.80,
                                  instrument = "LuZ",
                                  delta.depth = 2.5) {

  # Define the instument detection limit variable
  if (instrument == "EdZ") detect.lim = EdZ.detect.lim
  if (instrument == "EuZ") detect.lim = EuZ.detect.lim
  if (instrument == "LuZ") detect.lim = LuZ.detect.lim

  nwl       = dim(aop)[2]
  ndepth    = length(Depth)
  X.0m      = rep(NA,nwl) # Here X is used to replace either Ed, Lu or Eu
  Kx        = rep(NA,nwl)
  Z.interval= rep(NA,nwl)
  ix.Z.interval= rep(NA,nwl)
  r2.all = rep(NA,nwl)
  KomolSmirnov.p.value = rep(NA,nwl)

  # loop on each wavelength
  for (w in 1:nwl) {
    #
    print(paste("K linear for", dimnames(aop)[[2]][w]))

    max.depth = delta.depth + min(Depth, na.rm = T)
    ix.max.depth = which.min(abs(Depth - max.depth))
    min.depth = min(Depth, na.rm = T)+0.30

    # Check if there is any data above the detection limit
    ix.depth.interval = which(aop[1:ix.max.depth,w] > detect.lim[w])

    #### Increase the number of
    if (#length(ix.above.detection.limit) > 5 &
        length(ix.depth.interval) > 10) {

    # Start the extrapolation at 0.25 m below the first depth
    # and increment by 0.1 m up to delta.depth
    depth.intervals = seq(min.depth,max.depth,0.1)
    r2       = rep(NA, length(depth.intervals))
    sigma    = rep(NA, length(depth.intervals))
    ks.p.value= rep(NA, length(depth.intervals))
    X.0m.tmp = rep(NA, length(depth.intervals))
    Kx.tmp   = rep(NA, length(depth.intervals))
    ix.z     = rep(NA, length(depth.intervals))

    for (i in 1:length(depth.intervals)) {
      # find the index of the max depth to consider
      ix.z[i] = which.min(abs(Depth - depth.intervals[i]))

      # Get indices of measurement above detection limits within the layer
      ix.depth.interval = which(aop[1:ix.z[i],w] > detect.lim)
      n.good <- length(ix.depth.interval)
      if (n.good > 10) {
        df          = data.frame(z=Depth[ix.depth.interval],
                                 E=aop[ix.depth.interval,w])
        lmod        = lm(log(E) ~ z, data=df)
        r2[i]       = summary(lmod)$adj.r.squared
        sigma[i]    = summary(lmod)$sigma
        X.0m.tmp[i] = exp(coefficients(lmod)[1])
        Kx.tmp[i]   = -coefficients(lmod)[2]
        ##### apply a Kolmogorov-Smirnov test
        ##### to make sure the depth distribution
        ##### is not bimodal. It compare the Z vector
        ##### to an expected Z vector, which an evenly spaced
        ##### Z vector from the min and maxumum depth
        ##### If the p.value < 0.05, the Z vector is NOT
        ##### evenly spaced. This situation makes R^2 of linear
        ##### regression not valid
        expected     = seq(Depth[ix.depth.interval[1]],
                          Depth[ix.depth.interval[n.good]],
                          length.out = n.good)
        ks.res       = ks.test(df$z, expected)
        ks.p.value[i]= ks.res$p.value
      }
    }

    ##### remove fit that did not pass the Kolmogorov-Smirnov test
    #### The threshold is set to 0.1 but may need to be adjusted
    ix.valid <- which(ks.p.value > 0.10)
    if (length(ix.valid) > 1) {
      ##### Get the highest r2
      ix.max.r2 <- which.max(r2[ix.valid])

      ##### Check if the r2 is higher than the threshold
      if (r2[ix.valid[ix.max.r2]] > r2.threshold) {
        X.0m[w]          <- X.0m.tmp[ix.valid[ix.max.r2]]
        Kx[w]            <- Kx.tmp[ix.valid[ix.max.r2]]
        Z.interval[w]    <- depth.intervals[ix.valid[ix.max.r2]]
        ix.Z.interval[w] <- ix.z[ix.valid[ix.max.r2]]
        r2.all[w]        <- r2[ix.valid[ix.max.r2]]
        KomolSmirnov.p.value[w] <- ks.p.value[ix.valid[ix.max.r2]]
      } else {
        print("R-squared below the threshold")
      }
    } else {
      print("Kolmogorov-Smirnov test not passed at 0.1")
    }

    } else {
      print(paste("No measurements above the detection limit at", dimnames(aop)[[2]][w]))
    }

  }

  if (all(is.na(Z.interval))) {
    print("$$$$$$$$$$$$$$$$$$$$$")
    print("WARNING : No valid data for surface fitting")
    print("Check if it is a good profile.")
    print("It could be a Bioshade")
    print("$$$$$$$$$$$$$$$$$$$$$")
    return(list(X.0m= X.0m,
                Kx  = Kx,
                Z.interval=Z.interval,
                ix.Z.interval=ix.Z.interval,
                r2 = r2.all,
                KolmolSmirnov.p.value = KomolSmirnov.p.value))
  }


    return(list(X.0m= X.0m,
                Kx  = Kx,
                Z.interval=Z.interval,
                ix.Z.interval=ix.Z.interval,
                r2 = r2.all,
                KolmolSmirnov.p.value = KomolSmirnov.p.value))

  }
