

#library(tidyverse)

plot.station.means <- function(stationList = list("PME", "LDX", "BIN", "LDS", "PSO", "BSC", "RBS"), path2L2 = ".",
                               version = "4.4.2", mission = "AlgaeWISE") {

  load(paste("COPS.DB.PackageVersion", version, mission, "RData", sep = "."))

  df <- COPS.DB

  for (station in stationList) {


    waves <- df$waves
    dfTemp <- data.frame(waves = waves)
    subStations <- df$stationID[grep(station, df$stationID)]
    Kd <- df$Kd.pd.m[grep(station, df$stationID),]
    Rrs <- df$Rrs.m[grep(station, df$stationID),]

    pltKd <- ggplot(dfTemp, aes(x = waves))
    pltRrs <- ggplot(dfTemp , aes(x = waves))

    for (i in 1:length(subStations)) {

      dfTemp <- data.frame(waves = waves, subStation = rep(subStations[i], length(waves)), Kd = Kd[i,], Rrs = Rrs[i,])

      subDir <- list.files(path = ".", pattern = subStations[i])
      selectFile <- list.files(path = paste0(subDir[1], "/COPS/"), pattern = "select.cops.dat")
      select <- as.data.frame(read.table(file = paste0(subDir[1], "/COPS/", selectFile), sep = ";"))
      select <- select %>% filter(V4 > 0) %>% filter(V2 == 1)
      shallowCasts <- select$V1
      nShallowCasts <- length(shallowCasts)

      if (nShallowCasts > 0) {

        minShallowDepth <- Inf

        for (i in 1:nShallowCasts) {

          load(paste0(subDir[1], "/COPS/BIN/", shallowCasts[i], ".RData"))
          lastDepth <- length(cops$LuZ.anc$Depth)
          minShallowDepth <- min(minShallowDepth, cops$LuZ.anc$Depth[lastDepth])

        }

        dfTemp$subStation <- paste0(dfTemp$subStation, ": ", format(round(minShallowDepth, 2), nsmall = 2), "m")
      }

      pltKd <- pltKd + geom_line(data = dfTemp, aes(y = Kd, color = subStation))
      pltRrs <- pltRrs + geom_line(data = dfTemp, aes(y = Rrs, color = subStation))
    }

    pltRrs <- pltRrs + theme(legend.title = element_blank(), axis.title.y = element_text(size = 13),
                             axis.title.x = element_text(size = 14)) +
      xlab("\nWavelength (nm)") + ylab("Mean Rrs") +
      scale_y_continuous(labels = scientific)

    pltKd <- pltKd + theme(legend.title = element_blank(), axis.title.x = element_blank(),
                           axis.title.y = element_text(size = 13)) + ylab("Mean Kd.pd")

    plt <- pltKd / pltRrs + plot_annotation(title = station,
                                            theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_layout(guides = "collect")


    suppressMessages(ggsave(paste0(station, ".png"), plt, device = png, path = paste0(path2L2, "/station_mean_plots")))
    suppressMessages(plot(plt))

  }





}
