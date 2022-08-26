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


#library(ggplot2)
#library(patchwork)

plot.Rrs.Kd.for.station <- function(path = "./", depthEdZ = NA) {

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

  #setting up dataframe for plotting
  df <- data.frame(waves = waves[ix.waves])

  #plotting Rrs
  plotRrs <- ggplot(df, aes(x = waves))

  for (x in 1:nf) {

    print(paste("Plot Rrs and Kd for : ", listfile[x]))
    load(paste("./BIN/", listfile[x], ".RData", sep=""))

    df$Rrs.0p <- cops$Rrs.0p[ix.waves]

    df$Rrs.0p.linear <- cops$Rrs.0p.linear[ix.waves]

    #removing bad data
    maxR <- max(df$Rrs.0p, na.rm = TRUE)
    while (maxR > 1) {
      df$Rrs.0p[which.max(df$Rrs.0p)] <- NA
      maxR <- max(df$Rrs.0p, na.rm = TRUE)
    }

    maxRl <- max(df$Rrs.0p.linear, na.rm = TRUE)
    while (maxRl > 1) {
      df$Rrs.0p.linear[which.max(df$Rrs.0p.linear)] <- NA
      maxRl <- max(df$Rrs.0p.linear, na.rm = TRUE)
    }

    df$col <- rep(listfile[x], length(ix.waves))

    plotRrs <- plotRrs + geom_line(data = df, aes(y = Rrs.0p,
                                                color = col)) +
    geom_line(data = df, aes(y = Rrs.0p.linear,
                              color = col), linetype = "dashed")

  }

  plotRrs <- plotRrs +
    theme(legend.position = c(0.8, 0.82), legend.text = element_text(size = 8),
          axis.title.x = element_text(size = 13), legend.title = element_blank(),
          axis.title.y = element_text(size = 13)) +
    xlab("\nWavelength (nm)") + ylab("Rrs0+")


  #plotting Kd
  load(paste("./BIN/", listfile[1], ".RData", sep = ""))
  nbands <- length(cops$LuZ.waves)
  ix.depth <- rep(NA,nbands)
  K0.EdZ.fitted <- rep(NA, nbands)
  if (is.na(depthEdZ)) {
    if (anyNA(cops$EdZ.Z.interval[ix.waves])) {
      ix.2 <- which.min(abs(cops$depth.fitted - 2))
      K0.EdZ.fitted = cops$K0.EdZ.fitted[ix.2,]
    } else {
      for (w in 1:nbands) {
        if (!is.na(cops$EdZ.Z.interval[w])) {
          ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
          K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w], w]
        }
      }
    }
  } else {
    ix = which(cops$depth.fitted == depthEdZ)
    ix.depth <- rep(ix, nbands)
  }

  dfk <- data.frame(waves = waves, K0.EdZ.fitted = K0.EdZ.fitted)

  plotKd <- ggplot(dfk, aes(x = waves))

  plotKd <- plotKd + geom_line(aes(y = K0.EdZ.fitted))

  #plotting line for each cast
  for (x in 1:nf) {

    load(paste("./BIN/", listfile[x], ".RData", sep=""))
    dfk$K0.EdZ.fitted <- rep(NA, nbands)
    dfk$col <- rep(listfile[x], nbands)

    if (anyNA(cops$EdZ.Z.interval)) {
      print("Some invalid linear fit; plot Depth integrated K for the top 2m")
      ix.2 <- which.min(abs(cops$depth.fitted - 2))
      dfk$K0.EdZ.fitted <- cops$K0.EdZ.fitted[ix.2,]
    } else {
      for (w in 1:nbands) {
        if (!is.na(cops$EdZ.Z.interval[w])) {
          ix.depth[w] <- which.min(abs(cops$depth.fitted - cops$EdZ.Z.interval[w]))
          dfk$K0.EdZ.fitted[w] <- cops$K0.EdZ.fitted[ix.depth[w],w]
        }
      }
    }

    plotKd <- plotKd + geom_line(data = dfk, aes(x = waves, y = K0.EdZ.fitted,
                                                 color = col))

    if (!all(is.na(cops$EdZ.Z.interval))) {
      dfk$K.EdZ.surf <- cops$K.EdZ.surf
      plotKd <- plotKd + geom_line(data = dfk, aes(x = waves, y = K.EdZ.surf,
                                                   color = col),
                                   linetype = "dashed")
    }
  }


  #customizing graph
  plotKd <- plotKd +
    theme(legend.position = c(0.45, 0.82), legend.text = element_text(size = 8),
          legend.title = element_blank(), axis.title.y = element_text(size = 13),
          axis.title.x = element_blank()) +
    ylab(expression(K[0]~"("*E[d]*"z) linear versus loess"))

  dir <- dirname(getwd())
  fullpath <- unlist(strsplit(dir, "/"))
  station <- fullpath[length(fullpath)]

  #combining graphs (library(patchwork) required)
  fullplot <- plotKd / plotRrs + plot_annotation(title = station,
                                                 theme = theme(plot.title =
                                                                 element_text(hjust = 0.5)))

  suppressMessages(plot(fullplot))

  #suppressMessages(ggsave(paste("Kd", "and", "Rrs", "png", sep = "."), path = "./"))

  #suppressMessages(ggsave(paste0(station, ".png"), path = "/Users/charl/rstudio-workspace/AlgaeWISE/L2/Kd_and_Rrs"))


}
