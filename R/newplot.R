

newplot <- function(fullpath) {

  library(ggplot2)
  library(patchwork)
  library(scales)
  #filename=".....RData"

  splitu <- unlist(strsplit(fullpath, "_"))
  cast <- splitu[length(splitu) - 3]
 # if (missing(df)) {
    load(fullpath)
    waves <- cops$EdZ.waves
    ix.bottom <- length(cops$depth.fitted)
    EdZ.bottom <- cops$EdZ.fitted[ix.bottom,]
    EuZ.bottom = cops$EuZ.fitted[ix.bottom,]
    df <- data.frame(waves = waves,  EdZ.bottom = EdZ.bottom,
                     EuZ.bottom=EuZ.bottom)
    #assign(paste("df", cast, sep = ""), df, envir = .GlobalEnv)
  #}

  splits <- unlist(strsplit(fullpath, "/"))
  p <- splits[length(splits)]

  maxD <- max(df$EdZ.bottom, na.rm = TRUE)
  maxU <- max(df$EuZ.bottom, na.rm = TRUE)

  if (maxD > 1000) {
    df$EdZ.bottom[which.max(df$EdZ.bottom)] <- NA
    maxD <- max(df$EdZ.bottom, na.rm = TRUE)
  }
  if (maxU > 1000) {
    df$EuZ.bottom[which.max(df$EuZ.bottom)] <- NA
    maxU <- max(df$EdZ.bottom, na.rm = TRUE)
  }
  #scaler <- 0.1*maxD/maxU

  plot <- ggplot(df, aes(x = waves, y = EdZ.bottom, color = "EdZ")) +
    geom_line() +
    geom_line(aes(y = as.numeric(EuZ.bottom), color = "EuZ")) +
    scale_y_continuous(trans = "log", labels = scientific) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 14),
          legend.position = c(0.85,0.4),
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 14)) +
    xlab("\nWavelength (nm)") +
    ggtitle("Bottom Irrandiance") +
    scale_colour_manual(name = "Line Colour", values = c(EdZ = "black",
                                                         EuZ = "blue"))


  load(fullpath)
  if (cops$SHALLOW) {

    R.bottom <- cops$Rb.EuZ
    dfr <- data.frame(waves = waves, R.bottom = R.bottom, goodR.bottom = R.bottom)

    for (x in 1:length(dfr$goodR.bottom)) {
      if (!is.na(dfr$goodR.bottom[x]) && dfr$goodR.bottom[x] > 1)
        dfr$goodR.bottom[x] <- NA
    }

    max1 <- max(dfr$R.bottom, na.rm = TRUE)
    max2 <- max(dfr$goodR.bottom, na.rm = TRUE)
    scalerR <- max1 / max2

    plotR <- ggplot(dfr, aes(x = waves, y = R.bottom, color = "Rb")) +
      geom_line() + scale_y_continuous(sec.axis =
                                         sec_axis(trans = ~ . / scalerR)) +
      geom_line(aes(y = scalerR*as.numeric(goodR.bottom),
                    color = "Rb2")) +
      theme(axis.line.y.right = element_line(colour = 'blue'),
            axis.ticks.y.right = element_line(colour = 'blue'),
            axis.line.y.left = element_line(colour = 'black'),
            axis.ticks.y.left = element_line(colour = 'black'),
            axis.text.y.right = element_text(size = 12, colour = 'blue'),
            axis.text.y.left = element_text(size = 12, colour = 'black'),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.85,0.4),
            legend.text = element_text(size = 13),
            legend.title = element_text(size = 14)) +
      ggtitle("Calculated Bottom Reflectance") +
      scale_colour_manual(name = "Line Colour", values = c(Rb = "black",
                                                           Rb2 = "blue"))

    plot <- plotR / plot + plot_annotation(title = p,
      theme = theme(plot.title = element_text(hjust = 0.5)))

  }

  suppressWarnings(print(plot))

  destination <- paste(splits[1],splits[2],splits[3], sep = "/")

  splitp <- unlist(strsplit(p, "_"))
  name <- paste(splitp[1],splitp[2],splitp[3],splitp[4], sep = "_")

  suppressMessages(ggsave(paste(name, "png", sep = "."), path = "./bottom_spectra"))

}
