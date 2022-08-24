

plot.all.directories <- function() {

  directories <- list.files(path = ".", pattern = "Station",
             all.files = FALSE, full.names = TRUE)
  if (identical(directories, character(0))) print("Make sure to setwd(.../L2)")
  for (directory in directories) {
    p <- paste(directory, "COPS", "BIN", sep = "/")
    files <- list.files(path = p, pattern = ".*_CAST_.*RData$",
                        all.files = FALSE, full.names = FALSE)
    for (file in files) {
      newplot(paste(p, file, sep = "/"))
    }
  }
}
