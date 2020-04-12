#' COPS routine launcher
#'
#' @param interactive is a logical value: if TRUE, the user is prompted to
#' selected a folder containing COPS file to process. The plots are produced in pop-up windows.
#' If FALSE, you have to create an ASCII file named directories.for.cops.dat and put
#' the full path of the folders you want to process (on by line). The plots are put in a PDF file
#' in a sub-folder ./PDF.
#' @param ASCII is a logical value: if TRUE the outputs are written in simple ASCII files in a ddition to
#' the binary files in RData format.
#'
#' @author Simon Belanger
#' @export


cops.go <- function(interactive = FALSE, ASCII=FALSE) {
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
		if(!file.exists("directories.for.COPS.dat")) {
			cat("CREATE a file named directories.for.COPS.dat in current directory (where R is launched)\n")
			cat("  and put in it the names of the directories where data files can be found (one by line)\n")
			stop()
		} else {
			dirdats <- scan(file = "directories.for.COPS.dat", "", sep = "\n", comment.char = "#")
			for(dirdat in dirdats) {
				if(!file.exists(dirdat)) {
					cat(dirdat, "does not exist")
					stop()
				}
				mymessage(paste("PROCESSING DIRECTORY", dirdat), head = "@", tail = "@")
				process.cops(dirdat, ASCII)
				plot.Rrs.Kd.for.station(dirdat)
			}
		}
	}
}
