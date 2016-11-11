#
#    This function plot the Rrs and K0 at 1m depth for the cast retained
#    It reads the files "remove.cops.dat" and then load the RData file
#    that are stored in the "./BIN/" directory.
#
#   It produces 2 plots in PNG format in the  "./BIN/" directory.
#
#     By Simon Bélanger, UQAR, 23/02/2016

plot.Rrs.Kd.for.station <- function(plot.wd="./", depthEdZ = 1) {
  setwd(plot.wd)
  plot.wd = getwd()
  remove.file <- "remove.cops.dat"
  remove.tab <- read.table(remove.file, header = FALSE, colClasses = "character", sep = ";")
  kept.cast <- remove.tab[[2]] == "1"
  listfile  <- remove.tab[kept.cast, 1]
  nf = length(listfile)

  setwd("./BIN/")

  if (nf > 1) {


  load(paste(listfile[1], ".RData", sep=""))
  wl = cops$LuZ.waves
  nwl = length(wl)

  png(filename = "Rrs.png",
      width = 550, height = 480, units = "px",
      pointsize = 14, bg = "white")
  plot(wl[1:nwl], cops$Rrs.0p[1:nwl], xlab="Wavelenght", ylab="Rrs0+",
       ylim=c(0,max(cops$Rrs.0p[6:nwl],na.rm=T)+0.001),
       xlim=c(380, max(wl)),
       lwd=2, type="l", main=getwd(), sub=cops$dates[1])
  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    lines(wl[1:nwl], cops$Rrs.0p[1:nwl], col=i, lwd=2)
    lines(wl[1:nwl], cops$Rrs.0p.linear[1:nwl], col=i, lwd=2, lty=2)
  }

  legend("bottomleft", listfile, lwd=rep(2,nf), col=seq(nf))
  dev.off()

  load(paste(listfile[1], ".RData", sep=""))
  plot(wl, cops$Rrs.0p, xlab="Wavelenght",
       ylab="Rrs0+", xlim=c(380, max(wl)),
       lwd=2, type="l", main=getwd(), sub=cops$dates[1])
  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    lines(wl, cops$Rrs.0p, col=i, lwd=2)
    lines(wl, cops$Rrs.0p.linear, col=i, lwd=2, lty=2)
  }

  load(paste(listfile[1], ".RData", sep=""))
  ix = which(cops$depth.fitted == depthEdZ)
  png(filename = "Kd.png",
      width = 550, height = 480, units = "px",
      pointsize = 14, bg = "white")
  plot(wl, cops$K0.EdZ.fitted[ix,],
       xlab="Wavelenght", ylab="K0_Edz(1m)",
       lwd=2, type="l",  main=getwd(), sub=cops$dates[1])

  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    lines(wl[2:nwl], cops$K0.EdZ.fitted[ix,2:nwl], col=i, lwd=2)
    lines(wl[2:nwl], cops$K.EdZ.surf[2:nwl], col=i, lty=2, lwd=2)
  }

  legend("topright", listfile, lwd=rep(2,nf), col=seq(nf))
  dev.off()


  load(paste(listfile[1], ".RData", sep=""))
  plot(wl[2:nwl], cops$K0.EdZ.fitted[ix,2:nwl], xlab="Wavelenght", ylab="K0_Edz(1m)",
       lwd=2, type="l",  main=getwd(), sub=cops$dates[1])

  for (i in 1:nf){
    load(paste(listfile[i], ".RData", sep=""))
    lines(wl[2:nwl], cops$K0.EdZ.fitted[ix,2:nwl], col=i, lwd=2)
    lines(wl[2:nwl], cops$K.EdZ.surf[2:nwl], col=i, lty=2, lwd=2)
  }

}
  setwd(plot.wd)

}
