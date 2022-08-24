#'
#' This function finds all cast .RData files in the station
#' subdirectories, and calls a plotting function on each of them.
#'
#' This function must be launched from the L2 directory, which
#' should contain all processed data folders containing "Station"
#' in their name.
#'
#' The plotting function must have a parameter SAVE (boolean)
#'
#' @param functionName is an optional argument specifying the
#' name of the plotting function to be used. newplot() is called
#' by default
#'
#' @param save is an optional argument specifying whether the
#' generated plots should be saved in a folder called
#' "bottom_spectra" in the working directoy
#'
#' @author Charles-André Roux
#' @export

plot.all.directories <- function(functionName, save = F) {

  directories <- list.files(path = ".", pattern = "Station",
             all.files = FALSE, full.names = TRUE)
  if (identical(directories, character(0))) print("Make sure to setwd(.../L2)")
  for (directory in directories) {
    p <- paste(directory, "COPS", "BIN", sep = "/")
    files <- list.files(path = p, pattern = ".*_CAST_.*RData$",
                        all.files = FALSE, full.names = FALSE)
    if (missing(functionName)) {
      for (file in files) {
        newplot(paste(p, file, sep = "/"), SAVE = save)
      }
    } else {
      f <- get(functionName)
      for (file in files) {
        f(paste(p, file, sep = "/"), SAVE = save)
      }
    }
  }
}
