#'
#' Generate an AOPs data base derived from COPS light profiles for a list of directories
#'
#' @description The AOPs (Kd_s, Rrs) derived from valid profiles are averaged.
#' IMPORTANT: the only the cast selected in select.cops.dat will be average.
#' In addition, the extrapolation method for Rrs.0p must be specified in the
#' third column of the select.cops.dat.
#'
#' @param path is the path where the file directories.for.cops.dat containing the data folders
#' to merge in the data base.
#' @param wave.DB is a vector of wavelengths to include un the data base. Default is
#' waves.DB=c(305, 320, 330, 340,380, 412, 443, 465,490, 510, 532, 555,589, 625, 665, 683, 694, 710, 780)
#' @param mission is a string for the name of the mission. It will be used for the file names of the output.

#'
#' @return It returns a list object named COPS.DB containing matrices of
#' mean and standard deviation
#' of Kd1p, Kd10p, Kdpd, Rrs, Ed0.0p,  Ed0.f.diff
#' and vectors for
#' stationID, date, lat, lon sunzen and waves
#'
#' The object COPS.DB is saved in RData format. The data are also saved in ASCII (.dat with comma separator)
#' and a figure showing the measured rho_w spectra of the data base is produced.
#'
generate.cops.DB <- function(path="./",
                             mission="XXX") {

  GreggCarder.data()

  ##### All available wavelenghts proposed by BIOSPHERICAL
  waves.DB=c(305, 313, 320, 330, 340, 380, 395, 412, 443, 465, 490, 510, 532, 555,
             560, 565, 589, 625, 665, 683, 694, 710, 765, 780, 875)
  setwd(path)
  parentpath <- getwd()
  dirs <- scan(file = "directories.for.cops.dat", "", sep = "\n", comment.char = "#")

  ndirs = length(dirs)

  #
  nwaves = length(waves.DB)
  Rrs.m = matrix(ncol=nwaves, nrow = ndirs)
  nLw.m = matrix(ncol=nwaves, nrow = ndirs)
  Q.Factor.m = matrix(ncol=nwaves, nrow = ndirs)
  Rb.m = matrix(ncol=nwaves, nrow = ndirs)
  Rb.Q.m = matrix(ncol=nwaves, nrow = ndirs)
  Kd.1p.m = matrix(ncol=nwaves, nrow = ndirs)
  Kd.10p.m = matrix(ncol=nwaves, nrow = ndirs)
  Kd.pd.m = matrix(ncol=nwaves, nrow = ndirs)
  Ed0.0p.m = matrix(ncol=nwaves, nrow = ndirs)
  Rrs.sd = matrix(ncol=nwaves, nrow = ndirs)
  nLw.sd = matrix(ncol=nwaves, nrow = ndirs)
  Q.Factor.sd = matrix(ncol=nwaves, nrow = ndirs)
  Rb.sd = matrix(ncol=nwaves, nrow = ndirs)
  Rb.Q.sd = matrix(ncol=nwaves, nrow = ndirs)
  Kd.1p.sd = matrix(ncol=nwaves, nrow = ndirs)
  Kd.10p.sd = matrix(ncol=nwaves, nrow = ndirs)
  Kd.pd.sd = matrix(ncol=nwaves, nrow = ndirs)
  Ed0.0p.sd = matrix(ncol=nwaves, nrow = ndirs)
  Ed0.f =  matrix(0,ncol=(nwaves+1), nrow = ndirs)


  date = rep(NA, ndirs)
  stationID = rep("ID", ndirs)
  sunzen = rep(NA, ndirs)
  lat = rep(NA, ndirs)
  lon = rep(NA, ndirs)
  shadow.cor = rep(NA, ndirs)
  FU = rep(NA, ndirs)
  bottom.depth = rep(NA, ndirs)

  for (i in 1:ndirs) {
    print(paste("Extracting data from :", dirs[i]))

    setwd(as.character(dirs[i]))
    select.file <- "select.cops.dat"
    if (!file.exists(select.file)) {
      print("No select.cops.dat file exist")
      print("Data was processed using Cops package version 3.6 or lower")
      print("Data needs to be processed using Cops package version 4.0 or higher")
      print("Can not generate the Data Base. Exiting code")
      return(0)
    }
    select.tab <- read.table(select.file, header = FALSE, colClasses = "character", sep = ";")
    kept.cast <- select.tab[[2]] == "1"
    kept.bioS <- select.tab[[2]] == "2"
    IcePro    <- select.tab[[2]] == "3"  ### Nothing implemented for that type of cast so far.
    listfile  <- select.tab[kept.cast, 1]
    BioShadeFile = select.tab[kept.bioS, 1]

    #Select the extrapolation method to save Rrs
    Rrs_method <- select.tab[kept.cast, 3]
    SHALLOW <- select.tab[kept.cast, 4]

    setwd("./BIN/")

    nf = length(listfile)
    print(listfile)

    if (nf > 1) {

      mRrs      = matrix(ncol=nwaves, nrow = nf)
      mnLw      = matrix(ncol=nwaves, nrow = nf)
      mQ.Factor = matrix(ncol=nwaves, nrow = nf)
      mRb       = matrix(ncol=nwaves, nrow = nf)
      mRb.Q     = matrix(ncol=nwaves, nrow = nf)
      mKd1p     = matrix(ncol=nwaves, nrow = nf)
      mKd10p    = matrix(ncol=nwaves, nrow = nf)
      mKdpd     = matrix(ncol=nwaves, nrow = nf)
      mEd0.0p   = matrix(ncol=nwaves, nrow = nf)

      mdate = rep(NA, nf)
      msunzen = rep(NA, nf)
      mlat = rep(NA, nf)
      mlon = rep(NA, nf)
      mFU  = rep(NA, nf)
      mbottom.depth = rep(NA, nf)

      for (j in 1:nf) {

        if (Rrs_method[j] == "Rrs.0p.linear") LINEAR=TRUE else LINEAR=FALSE

        load(paste(listfile[j], ".RData", sep=""))
        waves = cops$Ed0.waves

        # Match the wavelengths of current data to the one requested for the database
        # it removes the NA values
        xw.DB <- match(waves, waves.DB)[!is.na(match(waves, waves.DB))]
        print("Matching wavelenghts: ")
        print(waves.DB[xw.DB])

        # Set the array indices for the current data in case some wavelengths are dropped
        xw <- match(waves.DB[xw.DB], waves)

        # extract ancillary info
        mdate[j] = cops$date.mean
        msunzen[j] = cops$sunzen
        mlat[j] = cops$latitude
        mlon[j] = cops$longitude

        # extract Rrs, nLw, Q-Factor, FU
        if (LINEAR) {
          mFU[j]  = cops$FU.linear
          mRrs[j,xw.DB] = cops$Rrs.0p.linear[xw]
          mnLw[j,xw.DB] = cops$nLw.0p.linear[xw]
          if (!is.null(cops$Q.linear)) mQ.Factor[j,xw.DB] = cops$Q.linear[xw]
        } else {
          mFU[j]  = cops$FU
          mRrs[j,xw.DB] = cops$Rrs.0p[xw]
          mnLw[j,xw.DB] = cops$nLw.0p[xw]
          if (!is.null(cops$Q)) mQ.Factor[j,xw.DB] = cops$Q[xw]
        }

        if (!is.na(SHALLOW[j])) {
          if (eval(parse(text=SHALLOW[j]))) mbottom.depth[j] <- cops$bottom.depth
          if (!is.null(cops$Rb.Q)) {
            mRb[j,xw.DB] = cops$Rb.EuZ[xw]
            mRb.Q[j,xw.DB] = cops$Rb.Q[xw]
          } else if (eval(parse(text=SHALLOW[j]))) mRb[j,xw.DB] = cops$Rb.LuZ[xw]
        }

        # extract Ed0.0p
        mEd0.0p[j, xw.DB] = cops$Ed0.0p[xw]

        # extract Kd1p, Kd10p Kdpd (mean Kd from surface to the first penetration depth
        # z_pd = (1/Kd) i.e when the %light is 36.7% of the surface
        # 1% and 10% light level)

        # find the depth of the 1% for each wavelength
        Ed0.0pm = matrix((0.97*cops$Ed0.0p[xw]), nrow=length(cops$depth.fitted), ncol=length(xw), byrow=T)
        percentEdZ =  cops$EdZ.fitted[,xw]/Ed0.0pm
        percentEdZ[is.na(percentEdZ)] <- 0
        ix.pair = seq(length(xw))*2
        z1 = apply(percentEdZ, 2, spline,y=cops$depth.fitted , xout=0.01)
        z1 = unlist(z1)[ix.pair]
        z1[z1<0] = NA
        z1[z1 > max(cops$depth.fitted)] = NA
        mKd1p[j,xw.DB] = 4.605/z1

        z10 = apply(percentEdZ, 2, spline,y=cops$depth.fitted , xout=0.1)
        z10 = unlist(z10)[ix.pair]
        z10[z10<0] = NA
        z10[z10 > max(cops$depth.fitted)] = NA
        mKd10p[j,xw.DB] = 2.3025/z10

        zpd = apply(percentEdZ, 2, spline,y=cops$depth.fitted , xout=0.3678)
        zpd = unlist(zpd)[ix.pair]
        zpd[zpd<0] = NA
        zpd[zpd > max(cops$depth.fitted)] = NA
        mKdpd[j,xw.DB] = 1/zpd

      }

      Rrs.stat = mean.parameter(mRrs[,xw.DB])
      nLw.stat = mean.parameter(mRrs[,xw.DB])
      Q.stat   = mean.parameter(mQ.Factor[,xw.DB])
      Rb.stat   = mean.parameter(mRb[,xw.DB])
      Rb.Q.stat = mean.parameter(mRb.Q[,xw.DB])
      Kd1p.stat = mean.parameter(mKd1p[,xw.DB])
      Kd10p.stat = mean.parameter(mKd10p[,xw.DB])
      Kdpd.stat = mean.parameter(mKdpd[,xw.DB])
      Ed0.0p.stat = mean.parameter(mEd0.0p[,xw.DB])

      # store the records in the global matrix
      Rrs.m[i,xw.DB] = Rrs.stat$mean
      nLw.m[i,xw.DB] = nLw.stat$mean
      Q.Factor.m[i,xw.DB] = Q.stat$mean
      Rb.m[i,xw.DB] = Rb.stat$mean
      Rb.Q.m[i,xw.DB] = Rb.Q.stat$mean
      Kd.1p.m[i,xw.DB]    = Kd1p.stat$mean
      Kd.10p.m[i,xw.DB]   = Kd10p.stat$mean
      Kd.pd.m[i,xw.DB]   = Kdpd.stat$mean
      Ed0.0p.m[i,xw.DB]   = Ed0.0p.stat$mean

      Rrs.sd[i,xw.DB] =      Rrs.stat$sd
      nLw.sd[i,xw.DB] = nLw.stat$sd
      Q.Factor.sd[i,xw.DB] = Q.stat$sd
      Rb.sd[i,xw.DB] = Rb.stat$sd
      Rb.Q.sd[i,xw.DB] = Rb.Q.stat$sd
      Kd.1p.sd[i,xw.DB]    = Kd1p.stat$sd
      Kd.10p.sd[i,xw.DB]   = Kd10p.stat$sd
      Kd.pd.sd[i,xw.DB]   = Kdpd.stat$sd
      Ed0.0p.sd[i,xw.DB]   = Ed0.0p.stat$sd

      date[i] = as.POSIXct(mean(mdate),origin = "1970-01-01")
      sunzen[i] =mean(msunzen)
      lon[i] = mean(mlon)
      lat[i] = mean(mlat)
      FU[i]  = mean(mFU)
      bottom.depth[i] = mean(mbottom.depth)

      if (is.na(cops$chl)) shadow.cor[i] <- "No correction"
      if (!is.na(cops$chl))shadow.cor[i] <- "abs.Case1.model"
      if (!is.na(cops$chl) & cops$chl == 999) shadow.cor[i] <- "abs.Kd.method"
      if (!is.na(cops$chl) & cops$chl == 0) shadow.cor[i] <- "abs.measured"


      mean.rec.data = c(Rrs.m[i,],
                        nLw.m[i,],
                        Q.Factor.m[i,],
                        Rb.m[i,],
                        Rb.Q.m[i,],
                        Kd.1p.m[i,] ,
                        Kd.10p.m[i,],
                        Kd.pd.m[i,],
                        Ed0.0p.m[i,])
      sd.rec.data = c(Rrs.sd[i,],
                      nLw.sd[i,],
                      Q.Factor.sd[i,],
                      Rb.sd[i,],
                      Rb.Q.sd[i,],
                      Kd.1p.sd[i,] ,
                      Kd.10p.sd[i,],
                      Kd.pd.m[i,],
                      Ed0.0p.sd[i,])

      rec.info = data.frame(date[i],sunzen[i], lat[i], lon[i], shadow.cor[i], FU[i], bottom.depth[i])

    } else {

      if (Rrs_method == "Rrs.0p.linear") LINEAR=TRUE else LINEAR=FALSE

      load(paste(listfile[1], ".RData", sep=""))
      waves = cops$Ed0.waves

      # Match the wavelengths of current data to the one requested for the database
      # it removes the NA values
      xw.DB <- match(waves, waves.DB)[!is.na(match(waves, waves.DB))]
      print("Matching wavelenghts: ")
      print(waves.DB[xw.DB])

      # Set the array indices for the current data in case some wavelenghts are dropped
      xw <- match(waves.DB[xw.DB], waves)

      # extract ancillary info
      date[i] = cops$date.mean
      sunzen[i] = cops$sunzen
      lat[i] = cops$latitude
      lon[i] = cops$longitude

      # extract Rrs, nLw, Q-Factor, FU
      if (LINEAR) {
        FU[i]  = cops$FU.linear
        Rrs.m[i,xw.DB] = cops$Rrs.0p.linear[xw]
        nLw.m[i,xw.DB] = cops$nLw.0p.linear[xw]
        if (!is.null(cops$Q.linear)) Q.Factor.m[i,xw.DB] = cops$Q.linear[xw]
      } else {
        FU[i]  = cops$FU
        Rrs.m[i,xw.DB] = cops$Rrs.0p[xw]
        nLw.m[i,xw.DB] = cops$nLw.0p[xw]
        if (!is.null(cops$Q)) Q.Factor.m[i,xw.DB] = cops$Q[xw]
      }

      if (!is.na(SHALLOW)) {
        bottom.depth[i] <- cops$bottom.depth
        if (!is.null(cops$Rb.Q)) {
          Rb.m[i,xw.DB] = cops$Rb.EuZ[xw]
          Rb.Q.m[i,xw.DB] = cops$Rb.Q[xw]
        } else Rb.m[i,xw.DB] = cops$Rb.LuZ[xw]
      }

      # extract Ed0.0p
      Ed0.0p.m[i, xw.DB] = cops$Ed0.0p[xw]

      # extract Kd1p and Kd10p (mean Kd fron surface to 1% and 10% light level)
      # find the depth of the 1% for each wavelength
      Ed0.0pm = matrix((0.97*cops$Ed0.0p[xw]), nrow=length(cops$depth.fitted), ncol=length(xw), byrow=T)
      percentEdZ =  cops$EdZ.fitted[,xw]/Ed0.0pm
      percentEdZ[is.na(percentEdZ)] <- 0
      ix.pair = seq(length(xw))*2
      z1 = apply(percentEdZ, 2, spline,y=cops$depth.fitted , xout=0.01)
      z1 = unlist(z1)[ix.pair]
      z1[z1<0] = NA
      z1[z1 > max(cops$depth.fitted)] = NA
      Kd.1p.m[i,xw.DB] = 4.605/z1

      z10 = apply(percentEdZ, 2, spline,y=cops$depth.fitted , xout=0.1)
      z10 = unlist(z10)[ix.pair]
      z10[z10<0] = NA
      z10[z10 > max(cops$depth.fitted)] = NA
      Kd.10p.m[i,xw.DB] = 2.3025/z10

      zpd = apply(percentEdZ, 2, spline,y=cops$depth.fitted , xout=0.3678)
      zpd = unlist(zpd)[ix.pair]
      zpd[zpd<0] = NA
      zpd[zpd > max(cops$depth.fitted)] = NA
      Kd.pd.m[i,xw.DB] = 1/zpd

      mean.rec.data = c(Rrs.m[i,],
                        nLw.m[i,],
                        Q.Factor.m[i,],
                        Rb.m[i,],
                        Rb.Q.m[i,],
                    Kd.1p.m[i,] ,
                   Kd.10p.m[i,],
                   Kd.pd.m[i,],
                    Ed0.0p.m[i,])
      sd.rec.data = c(rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves),
                      rep(NA,nwaves))

      if (is.na(cops$chl)) shadow.cor[i] <- "No correction"
      if (!is.na(cops$chl))shadow.cor[i] <- "abs.Case1.model"
      if (!is.na(cops$chl) & cops$chl == 999) shadow.cor[i] <- "abs.Kd.method"
      if (!is.na(cops$chl) & cops$chl == 0) shadow.cor[i] <- "abs.measured"

      rec.info = data.frame(date[i],sunzen[i], lat[i],
                            lon[i], shadow.cor[i], FU[i], bottom.depth[i])

    }

    if (length(BioShadeFile) != 0) {
      if (length(BioShadeFile) > 1) {
        print("More than one BioShade... use the first")
        BioShadeFile = BioShadeFile[1]
      }
      print(paste("Load BioShade file:", BioShadeFile))
      load(paste(BioShadeFile, ".RData", sep=""))

      Ed0.f[i,xw.DB] = cops$Ed0.f[xw]
      Ed0.f[i,(nwaves+1)] = 1

      mean.rec.data = c(mean.rec.data, Ed0.f[i,])

    } else {

      # No BIOSHADE
      print ("No Bioshade measurements")
      print ("Compute Ed0.dif/Ed0.tot using Gregg and Carder model")
      julian.day <- as.numeric(format(cops$date.mean, format = "%j"))
      visibility <- 25
      egc <- GreggCarder.f(julian.day, lon[i], lat[i], sunzen[i], lam.sel = waves.DB[xw.DB], Vi=visibility)

      ix.490 <- which.min(abs(waves.DB[xw.DB] - 490))

      ratio = egc$Ed[ix.490]*100/cops$Ed0.0p[ix.490]

      while (ratio > 1.05  & visibility > 0.5) {
        egc <- GreggCarder.f(julian.day, lon[i], lat[i], sunzen[i],lam.sel = waves.DB[xw.DB], Vi=visibility)
        ratio = egc$Ed[ix.490]*100/cops$Ed0.0p[ix.490]
        visibility = visibility - 0.5
      }
      Ed0.f[i,xw.DB] = egc$Edif/egc$Ed
      print("Visibility: ")
      print(visibility)
      mean.rec.data = c(mean.rec.data, Ed0.f[i,])
    }

    ####
    if (i == 1) {
      all = data.frame(rec.info,t(mean.rec.data), t(sd.rec.data))
      col.names = c(paste("Rrs_", waves.DB, "_mean",sep=""),
                    paste("nLw_", waves.DB, "_mean",sep=""),
                    paste("Q.Factor_", waves.DB, "_mean",sep=""),
                    paste("Rb_", waves.DB, "_mean",sep=""),
                    paste("Rb.Q_", waves.DB, "_mean",sep=""),
                    paste("Kd1p_", waves.DB,"_mean", sep=""),
                    paste("Kd10p_", waves.DB,"_mean", sep=""),
                    paste("Kdpd_", waves.DB,"_mean", sep=""),
                    paste("Ed0.0p_", waves.DB,"_mean", sep=""),
                    paste("Ed0.f.diff_", waves.DB,"_mean", sep=""), "Ed0.f.diff.measured",
                    paste("Rrs_", waves.DB, "_sd",sep=""),
                    paste("nLw_", waves.DB, "_sd",sep=""),
                    paste("Q.Factor_", waves.DB, "_sd",sep=""),
                    paste("Rb_", waves.DB, "_sd",sep=""),
                    paste("Rb.Q_", waves.DB, "_sd",sep=""),
                    paste("Kd1p_", waves.DB,"_sd", sep=""),
                    paste("Kd10p_", waves.DB,"_sd", sep=""),
                    paste("Kdpd_", waves.DB,"_sd", sep=""),
                    paste("Ed0.0p_", waves.DB,"_sd", sep=""))

      names(all) <- c("DateTime", "sunzen", "latitude", "longitude", "Shadow.correction.method", "FU", "Bottom.depth",col.names)
    } else
    {
      rec = data.frame(rec.info,t(mean.rec.data), t(sd.rec.data))
      names(rec) <-  c("DateTime", "sunzen", "latitude", "longitude", "Shadow.correction.method","FU", "Bottom.depth",col.names)
      all = rbind(all,rec)
    }

  }

  setwd(path)
  ### Extract the Station ID from the paths
  for (d in 1:ndirs) {
    res=unlist(strsplit(as.character(dirs[d]), "/"))
    for (i in 1:length(res)){
      xx = str_locate(res[i], "_Station")
      if (!is.na(xx[1])) {
        datestation=unlist(strsplit(res[i], "_Station"))
        # Remove "_" from station name to avoid error with Latex
        if (str_detect(datestation[2], "_")) {
          yy = unlist(strsplit(datestation[2], "_"))

          stationID[d] = paste(yy, collapse=" ")
        }   else stationID[d] =   datestation[2]
      }
    }
  }

  ##### Remove wavelenghts that are absent.
  to.remove = apply(is.na(Ed0.0p.m),2,all)
  ix.to.remove = which(to.remove)

  COPS.DB = list(stationID = stationID,
                 date = as.POSIXct(date,origin = "1970-01-01"),
                 sunzen = sunzen,
                 lat = lat,
                 lon = lon,
                 FU  = FU,
                 shadow.correction.method = shadow.cor,
                 bottom.depth = bottom.depth,
                 waves = waves.DB[-ix.to.remove],
                 Rrs.m = Rrs.m[,-ix.to.remove],
                 nLw.m = nLw.m[,-ix.to.remove],
                 Q.Factor.m = Q.Factor.m[,-ix.to.remove],
                 Rb.m = Rb.m[,-ix.to.remove],
                 Rb.Q.m = Rb.Q.m[,-ix.to.remove],
                 Kd.1p.m = Kd.1p.m[,-ix.to.remove],
                 Kd.10p.m = Kd.10p.m[,-ix.to.remove],
                 Kd.pd.m = Kd.pd.m[,-ix.to.remove],
                 Ed0.0p.m = Ed0.0p.m[,-ix.to.remove],
                 Rrs.sd = Rrs.sd[,-ix.to.remove],
                 nLw.sd = nLw.sd[,-ix.to.remove],
                 Q.Factor.sd = Q.Factor.sd[,-ix.to.remove],
                 Rb.sd = Rb.sd[,-ix.to.remove],
                 Rb.Q.sd = Rb.Q.sd[,-ix.to.remove],
                 Kd.1p.sd = Kd.1p.sd[,-ix.to.remove],
                 Kd.10p.sd = Kd.10p.sd[,-ix.to.remove],
                 Kd.pd.sd = Kd.pd.sd[,-ix.to.remove],
                 Ed0.0p.sd = Ed0.0p.sd[,-ix.to.remove],
                 Ed0.f.diff = Ed0.f[,c(-ix.to.remove,-(nwaves+1))],
                 Ed0.f.diff.measured = Ed0.f[,(nwaves+1)])

  all <- cbind(stationID,all)
  setwd(parentpath)
  save(COPS.DB, file = paste("COPS.DB.PackageVersion.",packageVersion("Cops"),".",mission,".RData", sep=""))
  write.table(all, file = paste("COPS.DB.PackageVersion.",packageVersion("Cops"),".",mission,".dat", sep=""), sep=",", quote=F, row.names=F)

  return(COPS.DB)
}

mean.parameter <- function(par) {
  mean.par = apply(par, 2, mean, na.rm=T)
  sd.par = apply(par, 2, sd, na.rm=T)
  return(list(mean=mean.par, sd=sd.par))
}
