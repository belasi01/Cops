.onLoad <- function(lib, pkg) {
	Sys.setenv(TZ = "GMT")
	Sys.setenv(R_COPS_DATA_DIR=paste(lib, pkg, "data", sep="/"))
	cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
	cat("TO BEGIN A PROCESSING AND PRODUCE WINDOWS PLOTS,\n")
	cat("    TYPE : cops.go(interactive = TRUE)\n")
	cat("TO BEGIN A PROCESSING AND PRODUCE   PDF   PLOTS,\n")
	cat("    TYPE : cops.go()\n")
	cat("TO BEGIN A PROCESSING AND PRODUCE   PDF   PLOTS, AND OUTPUT DATA IN ASCII FORMAT\n")
	cat("    TYPE : cops.go(ASCII=TRUE)\n")
	cat("TO BEGIN A PROCESSING AND PRODUCE   PDF   PLOTS, AND CLEAN THE PROFILE INTERACTIVELY\n")
	cat("    TYPE : cops.go(CLEAN.FILES=TRUE)\n")
	cat("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@\n")
}
