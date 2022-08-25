#'
#'    This function plot the replicates of Rrs and
#'    the integrated Kd (K0) from surface to a given depth.
#'
#' @descripton
#'    The retained casts have a value of 1 in the select.cops.dat.
#'    The code reads the files "select.cops.dat" and then load the RData file
#'    that are stored in the "./BIN/" directory.
#'
#' @param path is the path where raw COPS data are located.
#' @param depthEdZ is the depth of the ingetration of Kd.
#'    Default is NA and the code will take the same depth used for
#'    the linear interpolation.
#'
#' @return It produces 2 plots in PNG format in the working directory.
#'
#' @author  Simon Belanger, UQAR
#' @author Charles-André Roux
#' @export

new.plot.Rrs.Kd.for.station <- function(path = "./", depthEdZ = NA) {

  setwd(path)
  plot.wd = getwd()
  select.file <- "select.cops.dat"
  select.tab <- read.table(select.file, header = FALSE, colClasses = "character", sep = ";")
  kept.cast <- select.tab[[2]] == "1"
  listfile  <- select.tab[kept.cast, 1]
  nf = length(listfile)

  # read the first file
  if (nf == 0) {
    print("No Rrs to plot")
    return(0)
  }

  load(paste("./BIN/", listfile[1], ".RData", sep=""))
  waves = cops$LuZ.waves
  ix.waves <- which(waves > 370)

  df <- data.frame(waves = waves[ix.waves])

  for (x in 1:nf) {
    print(paste("Plot Rrs and Kd for : ", listfile[x]))
    load(paste("./BIN/", listfile[x], ".RData", sep=""))

    df[ncol(df) + 1] <- cops$Rrs.0p[ix.waves]
    colnames(df)[ncol(df)] <- paste0("Rrs.0p",x)

    df[ncol(df) + 1] <- cops$Rrs.0p.linear[ix.waves]
    colnames(df)[ncol(df)] <- paste0("Rrs.0p.linear",x)
  }

  #plotting Rrs
  plotRrs <- ggplot(df, aes(x = waves))

  plotRrs <- plotRrs + geom_line(data = df, aes(y = Rrs.0p1,
                                                col = listfile[1])) +
    geom_line(data = df, aes(y = Rrs.0p.linear1,
                              col = listfile[1]), linetype = "dashed")

  if (nf >= 2) {
    plotRrs <- plotRrs + geom_line(data = df, aes(y = Rrs.0p2,
                                                  col = listfile[2])) +
       geom_line(data = df, aes(y = Rrs.0p.linear2,
                                col = listfile[2]), linetype = "dashed")
  }
  if (nf >= 3) {
    plotRrs <- plotRrs + geom_line(data = df, aes(y = Rrs.0p3,
                                                  col = listfile[3])) +
      geom_line(data = df, aes(y = Rrs.0p.linear3,
                               col = listfile[3]), linetype = "dashed")
  }
  if (nf >= 4) {
    plotRrs <- plotRrs + geom_line(data = df, aes(y = Rrs.0p4,
                                                  col = listfile[4])) +
      geom_line(data = df, aes(y = Rrs.0p.linear4,
                               col = listfile[4]), linetype = "dashed")
    }

  plotRrs <- plotRrs +
    theme(legend.position = c(0.74, 0.92), legend.text = element_text(size = 8),
          axis.title.y = element_blank(), axis.title.x = element_text(size = 13),
          legend.title = element_blank()) +
    xlab("Wavelength (nm)")


  #plotting Kd
  load(paste("./BIN/", listfile[1], ".RData", sep = ""))
  ix.depth <- rep(NA,19)
  K0.EdZ.fitted <- rep(NA, 19)
  if (is.na(depthEdZ)) {
    if (anyNA(cops$EdZ.Z.interval[ix.waves])) {
      ix.2 <- which.min(abs(cops$depth.fitted - 2))
      K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2,]
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

  dfk <- data.frame(waves = waves, K0.EdZ.fitted = K0.EdZ.fitted)

  plotKd <- ggplot(dfk, aes(x = waves))

  plotKd <- plotKd + geom_line(aes(y = K0.EdZ.fitted))

  #first cast
  load(paste("./BIN/", listfile[1], ".RData", sep=""))
  K0.EdZ.fitted <- rep(NA, 19)

  if (anyNA(cops$EdZ.Z.interval)) {
    print("Some invalid linear fit; plot Depth integrated K for the top 2m")
    ix.2 <- which.min(abs(cops$depth.fitted - 2))
    K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2,]
  } else {
    for (w in 1:19) {
      if (!is.na(cops$EdZ.Z.interval[w])) {
        ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
        K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
      }
    }
  dfk <- data.frame(waves = waves, K0.EdZ.fitted = K0.EdZ.fitted)
  plotKd <- plotKd + geom_line(aes(y = K0.EdZ.fitted), col = "#F8766D")

  if (!all(is.na(cops$EdZ.Z.interval)))
    plotKd <- plotKd + geom_line(aes(y = cops$K.EdZ.surf),
                                 col = "#F8766D", linetype = "dashed")
}

  #second cast
  if (nf > 1) {
  load(paste("./BIN/", listfile[2], ".RData", sep=""))
  K0.EdZ.fitted <- rep(NA, 19)

  if (anyNA(cops$EdZ.Z.interval)) {
    print("Some invalid linear fit; plot Depth integrated K for the top 2m")
    ix.2 <- which.min(abs(cops$depth.fitted - 2))
    K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2,]
  } else {
    for (w in 1:19) {
      if (!is.na(cops$EdZ.Z.interval[w])) {
        ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
        K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
      }
    }
  dfk <- data.frame(waves = waves, K0.EdZ.fitted = K0.EdZ.fitted)
  plotKd <- plotKd + geom_line(aes(y = K0.EdZ.fitted), col = "#00BA38")

  if (!all(is.na(cops$EdZ.Z.interval)))
    plotKd <- plotKd + geom_line(aes(y = cops$K.EdZ.surf),
                                 col = "#00BA38", linetype = "dashed")
  }
}

  #third cast
  if (nf > 2) {
  load(paste("./BIN/", listfile[3], ".RData", sep=""))
  K0.EdZ.fitted <- rep(NA, 19)

  if (anyNA(cops$EdZ.Z.interval)) {
    print("Some invalid linear fit; plot Depth integrated K for the top 2m")
    ix.2 <- which.min(abs(cops$depth.fitted - 2))
    K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2,]
  } else {
    for (w in 1:19) {
      if (!is.na(cops$EdZ.Z.interval[w])) {
        ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
        K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
      }
    }
    dfk <- data.frame(waves = waves, K0.EdZ.fitted = K0.EdZ.fitted)

  plotKd <- plotKd + geom_line(aes(y = K0.EdZ.fitted), col = "#619CFF")

  if (!all(is.na(cops$EdZ.Z.interval)))
    plotKd <- plotKd + geom_line(aes(y = cops$K.EdZ.surf),
                                 col = "#619CFF", linetype = "dashed")
  }
}

  #fourth cast
  if (nf > 3) {
  load(paste("./BIN/", listfile[4], ".RData", sep=""))
  K0.EdZ.fitted <- rep(NA, 19)

  if (anyNA(cops$EdZ.Z.interval[ix.waves])) {
    print("Some invalid linear fit; plot Depth integrated K for the top 2m")
    ix.2 <- which.min(abs(cops$depth.fitted - 2))
    K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2,]
  } else {
    for (w in 1:19) {
      if (!is.na(cops$EdZ.Z.interval[w])) {
        ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
        K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
      }
    }
  }


  plotKd <- plotKd + geom_line(aes(y = K0.EdZ.fitted[ix.waves]), col = "purple")
  if (!all(is.na(cops$EdZ.Z.interval)))
    plotKd <- plotKd + geom_line(aes(y = cops$K.EdZ.surf[ix.waves]),
                                 col = "purple", linetype = "dashed")

}


  plotKd <- plotKd +
    theme(legend.position = c(0.74, 0.92), legend.text = element_text(size = 8),
                           axis.title.y = element_blank(), axis.title.x = element_text(size = 13),
                           legend.title = element_blank())

  fullplot <- plotKd / plotRrs

  plot(fullplot)

  suppressMessages(ggsave(paste("Kd", "and", "Rrs", "png", sep = "."), path = "./"))


}
