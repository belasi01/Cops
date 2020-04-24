#'
#'    This function plot the replicates of Rrs and
#'    the integrated Kd (K0) from surface to a given depth.
#'
#'    @descripton
#'    The retained casts have a value of 1 in the select.cops.dat.
#'    The code reads the files "select.cops.dat" and then load the RData file
#'    that are stored in the "./BIN/" directory.
#'
#'    @param path is the path where raw COPS data are located.
#'    @param depthEdZ is the depth of the ingetration of Kd.
#'    Default is NA and the code will take the same depth used for
#'    the linear interpolation.
#'
#'  @return It produces 2 plots in PNG format in the working directory.
#'
#' @author  Simon Belanger, UQAR
#' @export

plot.Rrs.Kd.for.station <- function(path="./", depthEdZ = NA) {
  setwd(path)
  plot.wd = getwd()
  select.file <- "select.cops.dat"
  select.tab <- read.table(select.file, header = FALSE, colClasses = "character", sep = ";")
  kept.cast <- select.tab[[2]] == "1"
  listfile  <- select.tab[kept.cast, 1]
  nf = length(listfile)

  # read the first file
  if (length(listfile) == 0) {
    print("No Rrs to plot")
    return(0)
  }
  print(paste("Plot Rrs and Kd for : ", listfile))
  setwd("./BIN/")
  load(paste(listfile[1], ".RData", sep=""))
  waves = cops$LuZ.waves
  nwaves = length(waves)

  # open a file
  png(filename = "../Rrs.png",
      width = 550, height = 480, units = "px",
      pointsize = 14, bg = "white")

  # select 380 to NIR bands
  ix.waves = which(waves > 370)
  plot(waves[ix.waves], cops$Rrs.0p[ix.waves], xlab="Wavelength",
       ylab="Rrs0+",
       ylim=c(0,max(cops$Rrs.0p[ix.waves],na.rm=T)+0.001),
       xlim=c(370, max(waves)+10),
       lwd=2, type="l", main=getwd(), sub=cops$date.mean)
  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    lines(waves[ix.waves], cops$Rrs.0p[ix.waves], col=i, lwd=2)
    lines(waves[ix.waves], cops$Rrs.0p.linear[ix.waves], col=i, lwd=2, lty=2)
  }

#  legend("topright", substr(listfile, regexpr("CAST", substr(listfile, 1, str_length(listfile[1])-4), fixed = T), regexpr("\\d\\d\\d", listfile)+2),
#         lwd=rep(2,nf), col=seq(nf), bg="transparent")
  legend("topright", substr(listfile, 1, str_length(listfile[1])-4),
                  lwd=rep(2,nf), col=seq(nf), bg="transparent", cex=0.8)
  dev.off()

  load(paste(listfile[1], ".RData", sep=""))
  plot(waves[ix.waves], cops$Rrs.0p[ix.waves], xlab="Wavelenght", ylab="Rrs0+",
       ylim=c(0,max(cops$Rrs.0p[ix.waves],na.rm=T)+0.001),
       xlim=c(370, max(waves)+10),
       lwd=2, type="l", main=getwd(), sub=cops$dates[1])
  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    lines(waves[ix.waves], cops$Rrs.0p[ix.waves], col=i, lwd=2)
    lines(waves[ix.waves], cops$Rrs.0p.linear[ix.waves], col=i, lwd=2, lty=2)
  }

  ##### plot Kd
  ix.waves = which(waves > 310)
  load(paste(listfile[1], ".RData", sep=""))
  ix.depth <- rep(NA,19)
  K0.EdZ.fitted <- rep(NA, 19)
  if (is.na(depthEdZ)) {
    if ( all(is.na(cops$EdZ.Z.interval))) {
      print("No valid linear fit; plot Depth integrated K for the top 2.5m")
      ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
      K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2.5,]
    } else {
      for (w in 1:19) {
        if (!is.na(cops$EdZ.Z.interval[w])) {
          ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
          K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
        }
      }
    }
  } else {
    ix = which(cops$depth.fitted == depthEdZ)
    ix.depth <- rep(ix,19)
  }

  png(filename = "../Kd.png",
      width = 550, height = 480, units = "px",
      pointsize = 14, bg = "white")

  plot(waves[ix.waves], K0.EdZ.fitted[ix.waves],
       xlab="Wavelenght",
       ylab=expression(K[0]~"("*E[d]*"z) linear versus loess"),
       lwd=2, type="l",  main=getwd(), sub=cops$date.mean)

  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    K0.EdZ.fitted <- rep(NA, 19)
    if (all(is.na(cops$EdZ.Z.interval))) {
      print("No valid linear fit; plot Depth integrated K for the top 2.5m")
      ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
      K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2.5,]
    } else {
      for (w in 1:19) {
        if (!is.na(cops$EdZ.Z.interval[w])) {
          ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
          K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
        }
      }
    }
    lines(waves[ix.waves], K0.EdZ.fitted[ix.waves], col=i, lwd=2)
    if (!all(is.na(cops$EdZ.Z.interval))) lines(waves[ix.waves], cops$K.EdZ.surf[ix.waves], col=i, lty=2, lwd=2)
  }
  legend("topright", substr(listfile, 1, str_length(listfile[1])-4),
         lwd=rep(2,nf), col=seq(nf), bg="transparent", cex=0.8)
   dev.off()

  ##### in RStudio
  load(paste(listfile[1], ".RData", sep=""))
  load(paste(listfile[1], ".RData", sep=""))
  ix.depth <- rep(NA,19)
  K0.EdZ.fitted <- rep(NA, 19)
  if (is.na(depthEdZ)) {
    if ( all(is.na(cops$EdZ.Z.interval))) {
      print("No valid linear fit; plot Depth integrated K for the top 2.5m")
      ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
      K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2.5,]
    } else {
      for (w in 1:19) {
        if (!is.na(cops$EdZ.Z.interval[w])) {
          ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
          K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
        }
      }
    }
  } else {
    ix = which(cops$depth.fitted == depthEdZ)
    ix.depth <- rep(ix,19)
  }

  plot(waves[ix.waves], K0.EdZ.fitted[ix.waves],
       xlab="Wavelenght",
       ylab=expression(K[0]~"("*E[d]*"z) linear versus loess"),
       lwd=2, type="l",  main=getwd(), sub=cops$date.mean)

  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    K0.EdZ.fitted <- rep(NA, 19)
    if (all(is.na(cops$EdZ.Z.interval))) {
      ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
      K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2.5,]
    } else {
      for (w in 1:19) {
        if (!is.na(cops$EdZ.Z.interval[w])) {
          ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
          K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
        }
      }
    }
    lines(waves[ix.waves], K0.EdZ.fitted[ix.waves], col=i, lwd=2)
    if (!all(is.na(cops$EdZ.Z.interval))) lines(waves[ix.waves], cops$K.EdZ.surf[ix.waves], col=i, lty=2, lwd=2)
  }
  legend("topright", substr(listfile, 1, str_length(listfile[1])-4),
         lwd=rep(2,nf), col=seq(nf), bg="transparent", cex=0.8)
  setwd(plot.wd)
}
