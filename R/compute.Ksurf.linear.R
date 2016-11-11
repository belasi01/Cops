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

  max.depth = delta.depth + min(Depth, na.rm = T)
  ix.max.depth = which.min(abs(Depth - max.depth))
  min.depth = min(Depth, na.rm = T)+0.5

  # loop on each wavelength
  for (w in 1:nwl) {
    #
    print(paste("K linear for", dimnames(aop)[[2]][w]))

    r2 = 0
    # Start the extrapolation at 0.5 m below the first depth
    z = which.min(abs(Depth - min.depth))

    # Check if there is any data above the detection limit
    ix.above.detection.limit <- which(aop[,w] > detect.lim)

    # Number of potential depth within the surface layer
    ix.depth.interval = which(aop[1:ix.max.depth,w] > detect.lim)


    if (length(ix.above.detection.limit) > 3 &
        length(ix.depth.interval) > 3) {
      # Increase the depth while the R2 of the linear regression
      # between  log(X) ~ z is below a given threshold
      while (r2 < r2.threshold &
             Depth[z] < max.depth &
             z < ndepth) {
        z = z + 1
        ix.depth.interval = which(aop[1:z,w] > detect.lim)

        if (length(ix.depth.interval) > 3) {
          df = data.frame(z=Depth[ix.depth.interval], E=aop[ix.depth.interval,w])
          lmod=lm(log(E) ~ z, data=df)
          r2= summary(lmod)$adj.r.squared
        }
      }
      X.0m[w]   <- exp(coefficients(lmod)[1])
      Kx[w]     <- -coefficients(lmod)[2]
      Z.interval[w]<- Depth[z]
      ix.Z.interval[w] <- z

    } else {
      print(paste("No measurements above the detection limit at", dimnames(aop)[[2]][w]))
    }


  }

    return(list(X.0m= X.0m,
                Kx  = Kx,
                Z.interval=Z.interval,
                ix.Z.interval=ix.Z.interval))

  }
