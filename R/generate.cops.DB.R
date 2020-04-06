#'
#' Generate an AOPs data base derived from COPS light profiles for a list of directories
#'
#' @description The AOPs (Kd_s, Rrs) derived from valid profiles are averaged.
#'
#' @param path is the path where the file directories.for.cops.dat containing the data folders
#' to merge in the data base.
#' @param wave.DB is a vector of wavelengths to include un the data base. Default is
#' waves.DB=c(305, 320, 330, 340,380, 412, 443, 465,490, 510, 532, 555,589, 625, 665, 683, 694, 710, 780)
#' @param mission is a string for the name of the mission. It will be used for the file names of the output.

#'
#' @return It returns a list object named COPS.DB containing matrices of
#' mean and standard deviation
#' of Kd1p, Kd10p, Rrs, Ed0.0p,  Ed0.f and vectors for
#' date, lat, lon sunzen and waves
#'
#' The object COPS.DB is saved in RData format. The data are also saved in ASCII (.dat with comma separator)
#' and a figure showing the measured rho_w spectra of the data base is produced.
#'
generate.cops.DB <- function(path="./",
                             waves.DB=c(320, 330, 340,
                                     380, 412, 443, 465,
                                     490, 510, 532, 555,
                                     589, 625, 665, 683,
                                     694, 710, 780, 875),
                             mission="XXX") {

  GreggCarder.data()
  setwd(path)
  dirs = read.table("directories.for.cops.dat")
  dirs = dirs$V1

  ndirs = length(dirs)

  #
nwaves = length(waves.DB)
  Rrs.m = matrix(ncol=nwaves, nrow = ndirs)
  Kd.1p.m = matrix(ncol=nwaves, nrow = ndirs)
  Kd.10p.m = matrix(ncol=nwaves, nrow = ndirs)
  Ed0.0p.m = matrix(ncol=nwaves, nrow = ndirs)
  Rrs.sd = matrix(ncol=nwaves, nrow = ndirs)
  Kd.1p.sd = matrix(ncol=nwaves, nrow = ndirs)
  Kd.10p.sd = matrix(ncol=nwaves, nrow = ndirs)
  Ed0.0p.sd = matrix(ncol=nwaves, nrow = ndirs)
  Ed0.f =  matrix(0,ncol=(nwaves+1), nrow = ndirs)


  date = rep(NA, ndirs)
  stationID = rep("ID", ndirs)
  sunzen = rep(NA, ndirs)
  lat = rep(NA, ndirs)
  lon = rep(NA, ndirs)
  shadow.cor = rep(NA, ndirs)

  for (i in 1:ndirs) {
    print(paste("Extracting data from :", dirs[i]))

    setwd(as.character(dirs[i]))
    remove.file <- "remove.cops.dat"
    remove.tab <- read.table(remove.file, header = FALSE, colClasses = "character", sep = ";")
    kept.cast <- remove.tab[[2]] == "1"
    kept.bioS <- remove.tab[[2]] == "2"
    listfile  <- remove.tab[kept.cast, 1]
    BioShadeFile = remove.tab[kept.bioS, 1]

    setwd("./BIN/")

    nf = length(listfile)
    print(listfile)

    if (nf > 1) {

      mRrs = matrix(ncol=nwaves, nrow = nf)
      mKd1p = matrix(ncol=nwaves, nrow = nf)
      mKd10p = matrix(ncol=nwaves, nrow = nf)
      mEd0.0p = matrix(ncol=nwaves, nrow = nf)

      mdate = rep(NA, nf)
      msunzen = rep(NA, nf)
      mlat = rep(NA, nf)
      mlon = rep(NA, nf)

      for (j in 1:nf) {

        load(paste(listfile[j], ".RData", sep=""))
        waves = cops$LuZ.waves

        # Match the wavelengths of current data to the one requested for the database
        # it removes the NA values
        xw.DB <- match(waves, waves.DB)[!is.na(match(waves, waves.DB))]
        print("Matching wavelenghts: ")
        print(waves.DB[xw.DB])

        # Set the array indices for the current data in case some wavelenghts are dropped
        xw <- match(waves.DB[xw.DB], waves)

        # extract ancillary info
        mdate[j] = cops$date.mean
        msunzen[j] = cops$sunzen
        mlat[j] = cops$latitude
        mlon[j] = cops$longitude

        # extract Rrs
        mRrs[j,xw.DB] = cops$Rrs.0p[xw]

        # extract Ed0.0p
        mEd0.0p[j, xw.DB] = cops$Ed0.0p[xw]

        # extract Kd1p and Kd10p (mean Kd fron surface to 1% and 10% light level)
        # find the depth of the 1% for each wavelength
        Ed0.0pm = matrix((0.97*cops$Ed0.0p[xw]), nrow=length(cops$depth.fitted), ncol=length(xw), byrow=T)
        percentEdZ =  cops$EdZ.fitted[,xw]/Ed0.0pm
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

      }

      Rrs.stat = mean.parameter(mRrs[,xw.DB])
      Kd1p.stat = mean.parameter(mKd1p[,xw.DB])
      Kd10p.stat = mean.parameter(mKd10p[,xw.DB])
      Ed0.0p.stat = mean.parameter(mEd0.0p[,xw.DB])

      # store the records in the global matrix
      Rrs.m[i,xw.DB] = Rrs.stat$mean
      Kd.1p.m[i,xw.DB]    = Kd1p.stat$mean
      Kd.10p.m[i,xw.DB]   = Kd10p.stat$mean
      Ed0.0p.m[i,xw.DB]   = Ed0.0p.stat$mean

      Rrs.sd[i,xw.DB] =      Rrs.stat$sd
      Kd.1p.sd[i,xw.DB]    = Kd1p.stat$sd
      Kd.10p.sd[i,xw.DB]   = Kd10p.stat$sd
      Ed0.0p.sd[i,xw.DB]   = Ed0.0p.stat$sd

      date[i] = mean(mdate)
      sunzen[i] =mean(msunzen)
      lon[i] = mean(mlon)
      lat[i] = mean(mlat)

      mean.rec.data = c(Rrs.m[i,],
                        Kd.1p.m[i,] ,
                        Kd.10p.m[i,],
                        Ed0.0p.m[i,])
      sd.rec.data = c(Rrs.sd[i,],
                      Kd.1p.sd[i,] ,
                      Kd.10p.sd[i,],
                      Ed0.0p.sd[i,])


      rec.info = data.frame(as.POSIXct(mean(mdate),origin = "1970-01-01"),
                            mean(msunzen), mean(mlat), mean(mlon))

    } else {

      load(paste(listfile[1], ".RData", sep=""))
      waves = cops$LuZ.waves

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

      # extract Rrs
      Rrs.m[i,xw.DB] = cops$Rrs.0p[xw]

      # extract Ed0.0p
      Ed0.0p.m[i, xw.DB] = cops$Ed0.0p[xw]

      # extract Kd1p and Kd10p (mean Kd fron surface to 1% and 10% light level)
      # find the depth of the 1% for each wavelength
      Ed0.0pm = matrix((0.97*cops$Ed0.0p[xw]), nrow=length(cops$depth.fitted), ncol=length(xw), byrow=T)
      percentEdZ =  cops$EdZ.fitted[,xw]/Ed0.0pm
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

      mean.rec.data = c(Rrs.m[i,],
                    Kd.1p.m[i,] ,
                   Kd.10p.m[i,],
                    Ed0.0p.m[i,])
      sd.rec.data = c(rep(NA,nwaves),rep(NA,nwaves),rep(NA,nwaves),rep(NA,nwaves))

      rec.info = data.frame(cops$date.mean,cops$sunzen, cops$latitude, cops$longitude)

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

    if (i == 1) {
      all = data.frame(rec.info,t(mean.rec.data), t(sd.rec.data))
      col.names = c(paste("Rrs_", waves.DB, "_mean",sep=""),
                    paste("Kd1p_", waves.DB,"_mean", sep=""),
                    paste("Kd10p_", waves.DB,"_mean", sep=""),
                    paste("Ed0.0p_", waves.DB,"_mean", sep=""),
                    paste("Ed0.f_", waves.DB,"_mean", sep=""), "Ed0.f.measured",
                    paste("Rrs_", waves.DB, "_sd",sep=""),
                    paste("Kd1p_", waves.DB,"_sd", sep=""),
                    paste("Kd10p_", waves.DB,"_sd", sep=""),
                    paste("Ed0.0p_", waves.DB,"_sd", sep=""))

      names(all) <- c("DateTime", "sunzen", "latitude", "longitude", col.names)
    } else
    {
      rec = data.frame(rec.info,t(mean.rec.data), t(sd.rec.data))
      names(rec) <-  c("DateTime", "sunzen", "latitude", "longitude", col.names)
      all = rbind(all,rec)
    }

    COPS.DB = list(waves = waves,
                        Rrs.m = Rrs.m,
                        Kd.1p.m = Kd.1p.m,
                        Kd.10p.m = Kd.10p.m,
                        Ed0.0p.m = Ed0.0p.m,
                        Rrs.sd = Rrs.sd,
                        Kd.1p.sd = Kd.1p.sd,
                        Kd.10p.sd = Kd.10p.sd,
                        Ed0.0p.sd = Ed0.0p.sd,
                        Ed0.f = Ed0.f[,(1:nwaves)],
                        Ed0.f.measured = Ed0.f[,(nwaves+1)],
                        date = as.POSIXct(date,origin = "1970-01-01"),
                        sunzen = sunzen,
                        lat = lat,
                        lon = lon)
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

  COPS.DB$stationID <- stationID
  save(COPS.DB, file = paste("COPS.DB.",mission,".RData", sep=""))
  write.table(all, file = paste("COPS.DB.",mission,".dat", sep=""), sep=",", quote=F, row.names=F)

  return(COPS.DB)
}

mean.parameter <- function(par) {
  mean.par = apply(par, 2, mean, na.rm=T)
  sd.par = apply(par, 2, sd, na.rm=T)
  return(list(mean=mean.par, sd=sd.par))
}
