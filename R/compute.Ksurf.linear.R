compute.Ksurf.linear <- function (Depth, aop,
                                  r2.threshold = 0.99,
                                  detect.lim = 5e-5,
                                  delta.depth = 2.5) {

  nwl       = dim(aop)[2]
  ndepth    = length(Depth)
  X.0m      = rep(NA,nwl) # Here X is used to replace either Ed, Lu or Eu
  Kx        = rep(NA,nwl)
  Z.interval= rep(NA,nwl)
  ix.Z.interval= rep(NA,nwl)

  # loop on each wavelength
  for (w in 1:nwl) {
    #
    print(paste("K linear for", dimnames(aop)[[2]][w]))

    max.depth = delta.depth + min(Depth, na.rm = T)
    ix.max.depth = which.min(abs(Depth - max.depth))
    min.depth = min(Depth, na.rm = T)+0.25

    # Check if there is any data above the detection limit
    ix.above.detection.limit <- which(aop[,w] > detect.lim)

    # Number of potential depth within the surface layer
    ix.depth.interval = which(aop[1:ix.max.depth,w] > detect.lim)


    if (length(ix.above.detection.limit) > 3 &
        length(ix.depth.interval) > 3) {

    # Start the extrapolation at 0.25 m below the first depth
    # and increment by 0.1 m up to delta.depth
    depth.intervals = seq(min.depth,max.depth,0.1)
    r2       = rep(NA, length(depth.intervals))
    X.0m.tmp = rep(NA, length(depth.intervals))
    Kx.tmp   = rep(NA, length(depth.intervals))
    ix.z     = rep(NA, length(depth.intervals))

    for (i in 1:length(depth.intervals)) {
      # find the index of the max depth to consider
      ix.z[i] = which.min(abs(Depth - depth.intervals[i]))

      # Get indices of measurement above detection limits within the layer
      ix.depth.interval = which(aop[1:ix.z[i],w] > detect.lim)

      if (length(ix.depth.interval) > 3) {
        df          = data.frame(z=Depth[ix.depth.interval], E=aop[ix.depth.interval,w])
        lmod        = lm(log(E) ~ z, data=df)
        r2[i]       = summary(lmod)$adj.r.squared
        X.0m.tmp[i] = exp(coefficients(lmod)[1])
        Kx.tmp[i]   = -coefficients(lmod)[2]
      }
    }

    ix.mar.r2 <- which.max(r2)
    X.0m[w]          <- X.0m.tmp[ix.mar.r2]
    Kx[w]            <- Kx.tmp[ix.mar.r2]
    Z.interval[w]    <- depth.intervals[ix.mar.r2]
    ix.Z.interval[w] <- ix.z[ix.mar.r2]

    } else {
      print(paste("No measurements above the detection limit at", dimnames(aop)[[2]][w]))
      depth.intervals = seq(min.depth,max.depth,0.1)
      r2       = rep(NA, length(depth.intervals))
    }

  }

    return(list(X.0m= X.0m,
                Kx  = Kx,
                Z.interval=Z.interval,
                ix.Z.interval=ix.Z.interval,
                r2 = r2[ix.mar.r2]))

  }
