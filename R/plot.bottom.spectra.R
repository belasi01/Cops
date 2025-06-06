#'
#' This function makes a plot of the upwelling and downwelling
#' irradiances at the deepest point of the cast. If the bottom
#' was reached (cops$SHALLOW = TRUE), then a plot of the
#' calculated bottom reflectance is generated above the initial
#' graph.
#'
#' @param fullpath is the path from the working directory to
#' the .RData file of the cast
#'
#' @param SAVE is a binary parameter specifying whether the
#' generated plot should be saved. Plots are saved in the
#' bottom_spectra folder of the working directory. Make sure to
#' create this directory beforehand.
#'
#' @author Charles-André Roux
#' @export
#'

plot.bottom.spectra <- function(cops, SAVE = FALSE, MaxThreshold_Ed_Eu = 1000) {


  # parsing the path string for the cast number
  fullpath <- paste(getwd(),"/BIN/", cops$file,'.RData', sep = "")
  splitu <- unlist(strsplit(fullpath, "_"))
  cast <- splitu[length(splitu) - 3]
  # parsing the path string for the .RData file name
  splits <- unlist(strsplit(fullpath, "/"))
  p <- splits[length(splits)]
  # getting station number
  pathstring <- normalizePath(fullpath)
  lst <- unlist(strsplit(pathstring, "/"))
  stationDir <- lst[length(lst) - 3]
  stationNum <- str_sub(stationDir, -1, -1)

  # initiating a dataframe used for plotting
  #load(fullpath)

  waves <- cops$EdZ.waves
  ix.bottom <- length(cops$depth.fitted)
  EdZ.bottom <- cops$EdZ.fitted[ix.bottom,]
  EuZ.bottom = cops$EuZ.fitted[ix.bottom,]
  df <- data.frame(waves = waves,  EdZ.bottom = EdZ.bottom,
                     EuZ.bottom=EuZ.bottom)

  # removing outlying data points from the data frame
  maxD <- max(df$EdZ.bottom, na.rm = TRUE)
  maxU <- max(df$EuZ.bottom, na.rm = TRUE)

  while (maxD > MaxThreshold_Ed_Eu) {
    df$EdZ.bottom[which.max(df$EdZ.bottom)] <- NA
    maxD <- max(df$EdZ.bottom, na.rm = TRUE)
  }
  while (maxU > MaxThreshold_Ed_Eu) {
    df$EuZ.bottom[which.max(df$EuZ.bottom)] <- NA
    maxU <- max(df$EdZ.bottom, na.rm = TRUE)
  }

  # generating initial plot
  plot <- ggplot(df, aes(x = waves, y = EdZ.bottom, color = "EdZ")) +
    geom_line() +
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

  if (sum(!is.na(df$EuZ.bottom)) > 1)
    plot <- plot + geom_line(data = df, aes(y = as.numeric(EuZ.bottom), color = "EuZ", group = 1))

  if (sum(!is.na(df$EuZ.bottom)) == 1)
    plot <- plot + geom_point(data = df, aes(y = as.numeric(EuZ.bottom), color = "EuZ"))


  #in case the bottom was reached
  if (cops$SHALLOW) {

    R.bottom <- cops$Rb.EuZ
    df$Rb <- R.bottom
    df$goodRb <- R.bottom

    maxR <- max(df$Rb, na.rm = TRUE)
    maxGR <- max(df$goodRb, na.rm = TRUE)

    #removing the values above 1 in the goodRb column
    while (maxGR > 1) {
      df$goodRb[which.max(df$goodRb)] <- NA
      maxGR <- max(df$goodRb, na.rm = TRUE)
    }

    #scaling factor for the sub-1 Rb data, so the relevant part of the
    #plot is visible
    scalerR <- maxR / maxGR
    secondNeeded <- maxR > 1

    plotR <- ggplot(df, aes(x = waves, y = Rb, color = "Rb")) +
      geom_line() +
      theme(axis.line.y.left = element_line(colour = 'black'),
            axis.ticks.y.left = element_line(colour = 'black'),
            axis.text.y.left = element_text(size = 12, colour = 'black'),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(size = 12),
            plot.title = element_text(hjust = 0.5),
            legend.position = c(0.85,0.4),
            legend.text = element_text(size = 12),
            legend.title = element_blank()) +
      ggtitle("Calculated Bottom Reflectance (Eu/Ed)") +
      scale_colour_manual(name = "Line Colour", values = c(Rb = "black"))

    if (secondNeeded) {
      suppressMessages(plotR <- plotR + scale_y_continuous(sec.axis =
                                    sec_axis(trans = ~ . / scalerR)) +
        geom_line(data = df, aes(y = scalerR*as.numeric(goodRb),
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
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 13)) +
        scale_colour_manual(name = "Line Colour", values = c(Rb = "black",
                                                             Rb2 = "blue")))

    }
    #adding the new plot to the existing one using the patchwork package
    plot <- plotR / plot

  }

  plot <- plot + plot_annotation(title = p,
                    theme = theme(plot.title = element_text(hjust = 0.5)))


  suppressWarnings(print(plot))


  if (SAVE) {
    splitp <- unlist(strsplit(p, "_"))
    if (!grepl("\\d", splitp[1]))
      splitp[1] <- paste0(splitp[1], stationNum)

    name <- paste(splitp[1],splitp[2],splitp[3],splitp[4], sep = "_")
    suppressMessages(ggsave(paste0(name, ".png")))
  }

}
