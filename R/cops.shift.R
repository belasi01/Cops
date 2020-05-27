cops.shift <- function(interactive = TRUE) {
	assign("INTERACTIVE", interactive, env = .GlobalEnv)
	data("thuillier.completed.by.AM0AM1")
	data("oz.k")
	data("table.Q")
	data("table.f")
	if(INTERACTIVE) {
		while(!is.na((dirdat <- tk_choose.dir()))) {
			mymessage(paste("PROCESSING DIRECTORY", dirdat), head = "@", tail = "@")
			on.file <- list.files(dirdat, pattern = "^on.waves.txt$", recursive = TRUE, full.names = TRUE)[1]
			old.waves <- read.table(on.file, header = TRUE)$Ed0.waves
			names(old.waves) <- 1:length(old.waves)
			print(old.waves)
			nidx <- c("idx410", "idx440", "idx490", "idx555", "idx670")
			selected <- NULL
			while(length(selected) != 5) {
				selected <- tk_select.list(old.waves, preselect = NULL, multiple = TRUE, title = "select the 5 wavelengths for QAA")
			}
			i.selected <- as.integer(names(selected))
			names(selected) <- nidx
			cat("you have selected", paste(i.selected, names(selected), selected, sep = ":"), sep = "\n")
			idx <- i.selected
			names(idx) <- nidx
			vals <- varEntryDialog(vars = "var1", labels="Enter new wavelengths, separated by \";\"")
			new.waves <- as.numeric(unlist(strsplit(vals$var1, ";")))
			print(new.waves)
			nearest.w <- list()
			nearest.i <- list()
			for(i in 1:length(new.waves)) {
				selected <- tk_select.list(old.waves, preselect = NULL, multiple = TRUE,
					title = paste("select the the nearest for wavelengths", new.waves[i]))
				cat(new.waves[i], ": nearest selected", selected, "\n")
				nearest.w <- c(nearest.w, list(as.numeric(selected)))
				nearest.i <- c(nearest.i, list(as.integer(names(selected))))
			}
			print(idx)
			for(i in 1:length(new.waves)) {
				cat(new.waves[i], ":", nearest.i[[i]], nearest.w[[i]], "\n")
			}
			nearest <- nearest.i
			shiftdir <- paste(dirdat, "shift.cops", sep = "/")
			if(!file.exists(shiftdir)) dir.create(shiftdir)
			oldwavesfile <- paste(shiftdir, "cops.waves.dat", sep = "/")
			cat(file = oldwavesfile, paste(1:length(old.waves), old.waves), sep = "\n")
			idxfile <- paste(shiftdir, "idx.dat", sep = "/")
			unlink(idxfile)
			for(i in 1:length(idx)) {
				cat(file = idxfile, append = TRUE, nidx[i], idx[i], "\n")
			}
			nearestfile <- paste(shiftdir, "nearest.dat", sep = "/")
			unlink(nearestfile)
			for(i in 1:length(new.waves)) {
				cat(file = nearestfile, append = TRUE,
					new.waves[i], length(nearest.i[[i]]), nearest.i[[i]], nearest.w[[i]], "\n")
			}
			init.file <- paste(dirdat, "init.cops.dat", sep = "/")
			cops.init00 <- read.init(init.file)
			process.shift(dirdat, idx, new.waves, nearest, bandwidth)
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
				on.file <- list.files(dirdat, pattern = "^on.waves.txt$", recursive = TRUE, full.names = TRUE)[1]
				old.waves <- read.table(on.file, header = TRUE)$Ed0.waves
				names(old.waves) <- 1:length(old.waves)
				print(old.waves)
				shiftdir <- paste(dirdat, "shift.cops", sep = "/")
				if(!file.exists(shiftdir)) stop("First, use cops.shift() in interactive mode")
				oldwavesfile <- paste(shiftdir, "cops.waves.dat", sep = "/")
				cat(file = oldwavesfile, paste(1:length(old.waves), old.waves), sep = "\n")
				idxfile <- paste(shiftdir, "idx.dat", sep = "/")
				tidx <- read.table(file = idxfile, header = FALSE, as.is = TRUE)
				idx <- tidx[[2]]
				names(idx) <- tidx[[1]]
				print(idx)
				nearestfile <- paste(shiftdir, "nearest.dat", sep = "/")
				tnearest <- scan(file = nearestfile, what = "", sep = "\n", quiet = TRUE)
				new.waves <- NULL
				nearest <- list()
				for(i in 1:length(tnearest)) {
					dum <- unlist(strsplit(tnearest[i], " "))
					new.waves <- append(new.waves, as.numeric(dum[1]))
					n <- as.integer(dum[2])
					k <- as.integer(dum[2 + 1:n])
					nearest <- c(nearest, list(k))
				}
				print(new.waves)
				print(nearest)
				init.file <- paste(dirdat, "init.cops.dat", sep = "/")
				cops.init00 <- read.init(init.file)
				process.shift(dirdat, idx, new.waves, nearest, bandwidth)
			}
		}
	}
}
