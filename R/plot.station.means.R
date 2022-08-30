#'
#' This function plots the means (provided by the generate.cops.DB function) of the Rrs and Kd for all substations
#' of the same name. (e.g. PME1, PME2, ...). It must be launched from the L2 directory, or be given the path to L2
#' in the fittingly named argument path2L2.
#'
#' @param stationName is a mandatory parameter for the name of the stations to be plotted. (e.g. PME, LDX, BIN, ...)
#'
#' @param path2L2 is a parameter indicating the path from the current working directory to the L2 directory, in which
#' the station folders are present. It is optional if the function is called from the L2 directory itself, and is set
#' to "." by default.
#'
#'
#' @returns the complete ggplot object of the graphs.
#'
#' @author Charles-Andre Roux
#'

plot.station.means <- function(stationName = NULL, path2L2 = ".") {


  here <- normalizePath(getwd())


  substations <- list.files(path = path2L2, pattern = paste0("Station", stationName))
  dir <- paste(here, substations[1], "COPS", sep = "\\")

  copsDB <- as.data.frame(generate.cops.DB(path = dir))
  setwd(here)

  df <- data.frame(waves = copsDB$waves, Rrs.m = copsDB$Rrs.m, Kd.pd.m = copsDB$Kd.pd.m, stationID = copsDB$stationID)



  pltKd <- ggplot(df, aes(x = waves))

  pltRrs <- ggplot(df, aes(x = waves))


  for (substation in substations) {

    dir <- paste(here, substation, "COPS", sep = "\\")

    rdatafiles <- list.files(path = paste(dir, "BIN", sep = "\\"), pattern = ".RData")
    bottomlist <- rep(NA, length(rdatafiles))

    for (i in 1:length(rdatafiles)) {

      load(paste(dir, "BIN", rdatafiles[i], sep = "\\"))
      lastDepth <- length(cops$LuZ.anc$Depth)
      bottom <- cops$LuZ.anc$Depth[lastDepth]
      if (is.null(bottom)) bottomlist[i] <- -Inf else bottomlist[i] <- bottom
    }

    maxBottom <- max(bottomlist)
    maxBottom <- format(round(maxBottom, 2), nsmall = 2)

    copsDB <- as.data.frame(generate.cops.DB(path = dir))
    setwd(here)

    copsDB$stationID <- paste0(copsDB$stationID, ": ", maxBottom, "m")
    dfTemp <- data.frame(waves = df$waves, Rrs.m = copsDB$Rrs.m, Kd.pd.m = copsDB$Kd.pd.m, stationID = copsDB$stationID)


    pltKd <- pltKd + geom_line(data = dfTemp, aes(y = Kd.pd.m, color = stationID))
    pltRrs <- pltRrs + geom_line(data = dfTemp, aes(y = Rrs.m, color = stationID))

  }

  pltRrs <- pltRrs + theme(legend.title = element_blank(), axis.title.y = element_text(size = 13),
                           axis.title.x = element_text(size = 14)) +
    xlab("\nWavelength (nm)") + ylab("Mean Rrs") +
    scale_y_continuous(labels = scientific)

  pltKd <- pltKd + theme(legend.title = element_blank(), axis.title.x = element_blank(),
                         axis.title.y = element_text(size = 13)) + ylab("Mean Kd.pd")

  plt <- pltKd / pltRrs + plot_annotation(title = stationName,
                                          theme = theme(plot.title = element_text(hjust = 0.5))) +
    plot_layout(guides = "collect")

  setwd(here)


  filename <- paste0(stationName, ".png")
  suppressMessages(ggsave(filename, plt, device = png, path = paste0(path2L2, "/station_mean_plots")))
  return(plt)

}
