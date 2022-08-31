#'
#' This function finds and calls a specified plotting function in all valid directories
#'
#' This function must be launched from the L2 directory, which
#' should contain all processed data folders containing "Station"
#' in their name.
#'
#'
#' @param functionName is an optional argument specifying the
#' name of the plotting function to be used. new.plot.Rrs.Kd.for.station is called
#' by default
#'
#' @author Charles-André Roux
#' @export

plot.all.Kd.Rrs <- function(functionName = "plot.Rrs.Kd.for.station") {

  here <- getwd()

  stations <- list.files(path = ".", pattern = "Station",
                            all.files = FALSE, full.names = TRUE)
  if (identical(stations, character(0))) print("Make sure to setwd(.../L2)")

  for (station in stations) {

    goodDir <- list.files(path = station, pattern = "COPS", all.files = FALSE,
                          full.names = FALSE)
    if (identical(goodDir, character(0))) next

    filesInCOPS <- list.files(path = paste(station, "COPS", sep = "/"),
                              pattern = ".dat", all.files = FALSE, full.names = FALSE)
    if (identical(filesInCOPS, character(0))) next

    f <- get(functionName)

    path <- paste(station, "COPS", sep = "/")
    f(path)
    setwd(here)
  }

}
