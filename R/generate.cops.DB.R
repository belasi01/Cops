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

nwaves = length(waves.DB)
  Rrs.m = c()
  Kd.1p.m = c()
  Kd.10p.m = c()
  Ed0.0p.m = c()
  Rrs.sd = c()
  Kd.1p.sd = c()
  Kd.10p.sd = c()
  Ed0.0p.sd = c()
  Ed0.f =  c()

  ID = c()
  date = c()
  stationID = c()
  sunzen = c()
  lat = c()
  lon = c()
  shadow.cor = c()
  Lambda = c()

  for (i in 1:ndirs) {
    print(paste("Extracting data from :", dirs[i]))
    ids <- str_extract(dirs[i], "(?<=Station).+(?=/COPS)")
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

      Rrs.sd =      append(Rrs.sd, Rrs.stat$sd)
      Kd.1p.sd   = append(Kd.1p.sd, Kd1p.stat$sd)
      Kd.10p.sd   = append(Kd.10p.sd, Kd10p.stat$sd)
      Ed0.0p.sd   = append(Ed0.0p.sd, Ed0.0p.stat$sd)

      # store the records in the global matrix
      Rrs.m = append(Rrs.m, Rrs.stat$mean)
      Kd.1p.m    = append(Kd.1p.m, Kd1p.stat$mean)
      Kd.10p.m   = append(Kd.10p.m, Kd10p.stat$mean)
      Ed0.0p.m   = append(Ed0.0p.m, Ed0.0p.stat$mean)

      ID = append(ID, rep(ids,nwaves))
      date = append(date,rep(mean(mdate),nwaves))
      sunzen = append(sunzen, rep(mean(msunzen),nwaves))
      lon = append(lon, rep(mean(mlon),nwaves))
      lat = append(lat, rep(mean(mlat),nwaves))
      Lambda = append(Lambda, waves)

      rec.info = data.frame(ID = ids, as.POSIXct(mean(mdate),origin = "1970-01-01"),
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

      rec.info = data.frame(ID = ids, cops$date.mean,cops$sunzen, cops$latitude, cops$longitude)

    }


    COPS.DB <- data.frame(ID, DateTime = as.POSIXct(date,origin = "1970-01-01"), lat, lon, sunzen, Lambda,
                          Rrs.m, Rrs.sd, Kd.1p.m, Kd.1p.sd, Kd.10p.m, Kd.10p.sd, Ed0.0p.m, Ed0.0p.sd)
  }


  setwd(path)

  #COPS.DB$stationID <- stationID
  save(COPS.DB, file = paste("COPS.DB.",mission,".RData", sep=""))
  write.table(all, file = paste("COPS.DB.",mission,".dat", sep=""), sep=",", quote=F, row.names=F)

  return(COPS.DB)
}

mean.parameter <- function(par) {
  mean.par = apply(par, 2, mean, na.rm=T)
  sd.par = apply(par, 2, sd, na.rm=T)
  return(list(mean=mean.par, sd=sd.par))
}
