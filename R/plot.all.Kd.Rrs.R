

plot.all.Kd.Rrs <- function(functionName) {

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

    f <- getFunction("new.plot.Rrs.Kd.for.station")
    if (!missing(functionName)) f <- get(functionName)

    path <- paste(station, "COPS", sep = "/")
    f(path)
    setwd(here)
  }

}
