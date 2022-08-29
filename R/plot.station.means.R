

plot.station.means <- function(stationName = NULL, path = ".") {


  here <- normalizePath(getwd())


  substations <- list.files(path = path, pattern = stationName)
  dir <- paste(here, substations[1], "COPS", sep = "\\")

  copsDB <- generate.cops.DB(path = dir)
  setwd(here)

  df <- as.data.frame(copsDB)
  plt <- ggplot(df, aes(x = waves))
  plt <- plt + geom_line(aes(y = Rrs.m, color = stationID))

  for (substation in substations) {

    dir <- paste(here, substation, "COPS", sep = "\\")

    copsDB <- generate.cops.DB(path = dir)
    setwd(here)


    dfTemp <- as.data.frame(copsDB)


    plt <- plt + geom_line(data = dfTemp, aes(y = Rrs.m, color = stationID))

  }

  plt <- plt + theme(legend.title = element_blank()) +
    xlab("\nWavelength (nm)") + ylab("Mean Rrs") +
    plot_annotation(title = stationName, theme = theme(plot.title =
                                                    element_text(hjust = 0.5)))

  setwd(here)


  suppressMessages(plot(plt))
  suppressMessages(ggsave(paste0(stationName, ".png"), "./station_mean_plots"))
}
