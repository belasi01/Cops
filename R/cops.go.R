cops.go <- function(interactive = TRUE) {
	assign("INTERACTIVE", interactive, env = .GlobalEnv)
	data("eu.hydrolight")
	data("thuillier.completed.by.AM0AM1")
	data("oz.k")
	data("table.Q")
	data("table.f")
	GreggCarder.data()
	shadow.data()
	if(INTERACTIVE) {
		while(!is.na((dirdat <- tk_choose.dir()))) {
			mymessage(paste("PROCESSING DIRECTORY", dirdat), head = "@", tail = "@")
			process.cops(dirdat)
		}
	} else {
		if(!file.exists("directories.for.cops.dat")) {
			cat("CREATE a file named directories.for.cops.dat in current directory (where R is launched)\n")
			cat("  and put in it the names of the directories where data files can be found (one by line)\n")
			stop()
		} else {
			dirdats <- scan(file = "directories.for.cops.dat", "", sep = "\n", comment.char = "#")
			for(dirdat in dirdats) {
				if(!file.exists(dirdat)) {
					cat(dirdat, "does not exist")
					stop()
				}
				mymessage(paste("PROCESSING DIRECTORY", dirdat), head = "@", tail = "@")
				process.cops(dirdat)
				plot.Rrs.Kd.for.station(dirdat)
			}
		}
	}
}
