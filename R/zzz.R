.onLoad <- function(lib, pkg) {
	Sys.setenv(TZ = "GMT")
	Sys.setenv(R_COPS_DATA_DIR=paste(lib, pkg, "data", sep="/"))
	cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
	cat("TO BEGIN A PROCESSING AND PRODUCE WINDOWS PLOTS, TYPE : cops.go()\n")
	cat("TO BEGIN A PROCESSING AND PRODUCE   PDF   PLOTS,     TYPE : cops.go(interactive = FALSE)\n")
	cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
}
