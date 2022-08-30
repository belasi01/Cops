#' QWIP: A Quantitative Metric for Quality Control of Aquatic Reflectance
#' Spectral Shape Using the Apparent Visible Wavelength (Dierssen et al., FRS, 2022)
#'
#' @description This function calculates the Quality Water Index Polynomial (QWIP)
#' based on Diesrssen et al 2022 paper.
#'
#' @param Waves
#' @param Rrs
#'
#' @return QWIP.score
#'
#' @author Simon Bélanger
#' @export


QWIP <- function(Waves, Rrs, PLOT = TRUE, LABEL = "Station") {


  # Compute the Apparent Visible Wavelength (AVW)
  Waves.int <- (400:700)
  Rrs.int <- spline(Waves, Rrs, xout=Waves.int, method = "natural")$y

  AVW     <- sum(Rrs.int)/sum(Rrs.int/Waves.int)

  # Compute Normalized Difference Index (NDI)
  ix.492 <- which(Waves.int == 492)
  ix.560 <- which(Waves.int == 560)
  ix.665 <- which(Waves.int == 665)
  NDI     <- (Rrs.int[ix.665] - Rrs.int[ix.492])/(Rrs.int[ix.665] + Rrs.int[ix.492])

  # Compute the QWIP
  p1 <- -8.399885e-9
  p2 <- 1.715532e-5
  p3 <- -1.301670e-2
  p4 <- 4.357838e0
  p5 <- -5.449532e2
  QWIP <- p1*(AVW^4) + p2*(AVW^3) + p3*(AVW^2) + p4*AVW   + p5

  # QWIP Score
  QWIP.score = NDI - QWIP

  if (abs(QWIP.score) < 0.1) QWIP.PASS <- TRUE else QWIP.PASS <- FALSE

  predicted.AVW <- 440:600
  predicted.NDI <- p1*(predicted.AVW^4) +
                   p2*(predicted.AVW^3) +
                   p3*(predicted.AVW^2) +
                   p4*predicted.AVW   + p5

  # Classified the spectrum
  if (Rrs.int[ix.665] > Rrs.int[ix.560] | Rrs.int[ix.665] > 0.025) {
    class = "Red"
    class.col = "#c80d0d"
  } else {
    if (Rrs.int[ix.560] < Rrs.int[ix.492]) {
      class = "Blue"
      class.col = "#0888e0"
    } else {
        class = "Green"
        class.col = "#47bf13"
    }
  }

  # Get FU color scale
  FU <- Rrs2FU(Waves, Rrs)$FU


  # Plot
  if (PLOT) {

    df <- data.frame(AVW = predicted.AVW,
                     NDI = predicted.NDI,
                     NDI.minus.0.1 = predicted.NDI-0.1,
                     NDI.plus.0.1 = predicted.NDI+0.1,
                     NDI.minus.0.2 = predicted.NDI-0.2,
                     NDI.plus.0.2 = predicted.NDI+0.2)

    df.rrs <- data.frame(AVW = AVW,
                         NDI = NDI,
                         class.col = class.col)

    dfm <- melt(df,id.vars = "AVW")
    names(dfm) <- c("AVW", "Predicted", "NDI")

    p <- ggplot(dfm, aes(x=AVW, y=NDI)) + geom_line(aes(color=Predicted)) +
      geom_point(data=df.rrs, aes(x=AVW, y=NDI, color=class.col),size=3) +
      scale_color_manual(name="NDI",labels=c(LABEL, "Predicted", "-0.1", "+0.2", "-0.1", "+0.2"),
                         values=c(class.col,"black", "orange",  "red", "orange", "red"))

    if (QWIP.PASS) {
      p <- p + ggtitle(expression(R[rs]~" passed Quality Control"),
                       subtitle = paste("Water class:", class, "      FU color scale:", FU)) +

               labs(caption = "Method from Dierssen et al. Front. Rem. Sens. (2022)") +
        theme(plot.title = element_text(size=18, face="bold"),
              plot.subtitle = element_text(color = class.col, size=12, face="bold"))
    } else  {
      p <- p + ggtitle(expression(R[rs]~"Rrs failed Quality Control ; Check extrapolation"),
                       subtitle = paste("Water class:", class, "      FU color scale:", FU)) +
        labs(caption = "Method from Dierssen et al. Front. Rem. Sens. (2022)") +
        theme(plot.title = element_text(color="#c80d0d", size=18, face="bold.italic"),
              plot.subtitle = element_text(color = class.col, size=12, face="bold"))
    }

    print(p)



  }

}
