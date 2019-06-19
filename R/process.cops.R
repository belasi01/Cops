process.cops <- function(dirdat, ASCII=FALSE) {
	# initialisation file

	generic.init.file <- paste( Sys.getenv("R_COPS_DATA_DIR"), "init.parameters.dat", sep = "/")

	init.file <- paste(dirdat, "init.cops.dat", sep = "/")
	if(!file.exists(init.file)) {
		file.copy(from = generic.init.file, to = init.file)
		cat("EDIT file", init.file, "and CUSTOMIZE IT\n")
		stop()
	}
	cops.init00 <- read.init(init.file)

	# information file
	header.info.file <- paste(Sys.getenv("R_COPS_DATA_DIR"), "info.header.dat", sep = "/")
	info.file <- paste(dirdat, "info.cops.dat", sep = "/")
	if(!file.exists(info.file)) {
		file.copy(from = header.info.file, to = info.file)
		files.in.dirdat <- list.files(dirdat)
		files.in.dirdat <- files.in.dirdat[! files.in.dirdat %in% c("init.cops.dat", "info.cops.dat")]
		lines.in.info.file <- paste(files.in.dirdat, "NA", "NA", "999", "x", "x", "x","x", sep = ";")
		write(file = info.file, lines.in.info.file, append = TRUE, ncolumns = 1)
		cat("EDIT file", info.file, "and CUSTOMIZE IT\n")
		cat("  this file must contain as much lines as cops-experiments you want to process\n")
		cat("  you will find a header with instructions to fill this file\n")
		stop()
	}
	info.tab <- read.table(info.file,
		colClasses = c(
			"character", "numeric", "numeric", "numeric",
			"character", "character", "character", "character",
			"character", "character", "character", "character"),
		col.names = c("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "b1", "b2", "b3", "b4"),
		header = FALSE, fill = TRUE, sep = ";"
	)

	# removal file
	remove.file <- paste(dirdat, "remove.cops.dat", sep = "/")
	if(!file.exists(remove.file)) {
		write.table(file = remove.file, cbind(info.tab[, 1], "1"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
	}
	remove.tab <- read.table(remove.file, header = FALSE, colClasses = "character", sep = ";")
	if(!isTRUE(all.equal(remove.tab[[1]], info.tab[[1]]))) stop("info.cops.dat remove.cops.dat non compatibles")

	# absorption file
	absorption.file <- paste(dirdat, "absorption.cops.dat", sep = "/")
	if(!file.exists(absorption.file)) {
		write.table(file = absorption.file, cbind("cops.file", "wavelength1", "wavelength2", "..."), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
		write.table(file = absorption.file, append = TRUE, cbind(info.tab[, 1], "a1", "a2", "..."), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
	}
	absorption.tab <- read.table(absorption.file, header = TRUE, sep = ";", as.is = TRUE, stringsAsFactors = FALSE, check.names = FALSE, row.names = 1, fill = TRUE)
	if(!isTRUE(all.equal(rownames(absorption.tab), info.tab[[1]]))) stop("info.cops.dat absorption.cops.dat non compatibles")
str(absorption.tab)

	# RData results directory
	dirres <- paste(dirdat, "BIN", sep = "/")
	if(!file.exists(dirres)) dir.create(dirres)

	# ascii results directory
	if (ASCII) {
	  dirasc <- paste(dirdat, "ASC", sep = "/")
	  if(!file.exists(dirasc)) dir.create(dirasc)
	}


	# pdf directory
	dirpdf <- paste(dirdat, "PDF", sep = "/")
	if(!file.exists(dirpdf)) dir.create(dirpdf)

	#kept <- remove.tab[[2]] != "0"
	kept.cast <- remove.tab[[2]] == "1"
	kept.bioS <- remove.tab[[2]] == "2"
	kept <- kept.cast | kept.bioS
	cat("discarded experiments", info.tab[!kept, 1], "\n")
	cat("processed Profile experiments", info.tab[kept.cast, 1], "\n")
	cat("processed BioShade experiments", info.tab[kept.bioS, 1], "\n")
	info.tab <- info.tab[kept, ]
	remove.tab <- remove.tab[kept, ]

	#### order the file to process to allow Shadow Band processing first
	ix <- sort.int(as.numeric(remove.tab$V2), decreasing = T, index.return = T)$ix
	remove.tab<-remove.tab[ix,]
	info.tab<-info.tab[ix,]

	assign("dirres", dirres, env = .GlobalEnv)
	assign("dirdat", dirdat, env = .GlobalEnv)
	assign("dirpdf", dirpdf, env = .GlobalEnv)
	assign("info.file", info.file,  env = .GlobalEnv)
	assign("remove.file", remove.file,  env = .GlobalEnv)
	assign("info.tab", info.tab,  env = .GlobalEnv)
	assign("remove.tab", remove.tab,  env = .GlobalEnv)



	if(all(!kept)) {
		cat("NOTHING TO DO\n")
		stop()
	}
	experiments <- nrow(info.tab)
	for(experiment in 1:experiments) {

    if (remove.tab[experiment,2] == "1") {
      mymessage(paste("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "blacks"), head = "#", tail = "-")
      mymessage(paste(unlist(info.tab[experiment, ]), collapse = " "), tail = "#")
      # initialization
      cops.init <- cops.init00
      # line of info.file.dat
      # fields 1 to 4
      cops.file <- info.tab[experiment, 1]
      info.longitude <- info.tab[experiment, 2]
      info.latitude <- info.tab[experiment, 3]
      chl <- info.tab[experiment, 4]
      SHADOW.CORRECTION <- FALSE
      absorption.values <- NA
      absorption.waves <- NA
      if(!is.na(chl)) {
        SHADOW.CORRECTION <- TRUE
        if(chl < 0.000001) { #### Use the absorption values provided
          absorption.values <- unlist(absorption.tab[cops.file, ])
          absorption.waves <- as.numeric(names(absorption.tab))
          chl <- NA
        } else { # Use an estimation of absorption
          absorption.values <- NA
          absorption.waves <- NA
        }
      }
      # fields 5 to 8
      info5 <- info.tab[experiment, 5]
      if(info5 != "x") {
        cat("time.window modified in info.cops.dat file", time.window, "---> ")
        time.window <- as.numeric(unlist(strsplit(info5, ",")))
        cops.init$time.window <- time.window
        assign("time.window", time.window, env = .GlobalEnv)
        rm(time.window)
        cat(time.window, "\n")
      }

      info6 <- info.tab[experiment, 6]
      if(info6 != "x") {
        cat("sub.surface.removed.layer.optics modified in info.cops.dat file", sub.surface.removed.layer.optics, "---> ")
        sub.surface.removed.layer.optics <- as.numeric(unlist(strsplit(info6, ",")))
        names(sub.surface.removed.layer.optics) <- instruments.optics
        cops.init$sub.surface.removed.layer.optics <- sub.surface.removed.layer.optics
        assign("sub.surface.removed.layer.optics", sub.surface.removed.layer.optics, env = .GlobalEnv)
        rm(sub.surface.removed.layer.optics)
        cat(sub.surface.removed.layer.optics, "\n")
      }

      info7 <- info.tab[experiment, 7]
      if(info7 != "x") {
        cat("tiltmax.optics modified in info.cops.dat file", tiltmax.optics, "---> ")
        tiltmax.optics <- as.numeric(unlist(strsplit(info7, ",")))
        names(tiltmax.optics) <- instruments.optics
        cops.init$tiltmax.optics <- tiltmax.optics
        assign("tiltmax.optics", tiltmax.optics, env = .GlobalEnv)
        rm(tiltmax.optics)
        cat(tiltmax.optics, "\n")
      }

      info8 <- info.tab[experiment, 8]
      if(info8 != "x") {
        cat("time.interval.for.smoothing.optics modified in info.cops.dat file", time.interval.for.smoothing.optics, "---> ")
        time.interval.for.smoothing.optics <- as.numeric(unlist(strsplit(info8, ",")))
        names(time.interval.for.smoothing.optics) <- instruments.optics
        cops.init$time.interval.for.smoothing.optics <- time.interval.for.smoothing.optics
        assign("time.interval.for.smoothing.optics", time.interval.for.smoothing.optics, env = .GlobalEnv)
        rm(time.interval.for.smoothing.optics)
        cat(time.interval.for.smoothing.optics, "\n")
      }
      if(verbose) str(cops.init)
      # blacks
      blacks <- info.tab[experiment, 9:12]
      blacks <- blacks[blacks != ""]
      cops.info <- list(file = cops.file, chl = chl, SHADOW.CORRECTION = SHADOW.CORRECTION, absorption.waves = absorption.waves, absorption.values = absorption.values, blacks = blacks)
      if(verbose) str(cops.info)

      save.file <- paste(dirres, paste(cops.file, "RData", sep = "."), sep = "/")
      if (ASCII) {
        asc.filedir <- paste(dirasc, paste(cops.file, "asc", sep = "."), sep = "/")
        if(!file.exists(asc.filedir)) dir.create(asc.filedir)
      }
      pdf.file <- paste(dirpdf, paste(cops.file, "pdf", sep = "."), sep = "/")
      if(!INTERACTIVE) {
        pdf(file = pdf.file, width = 11, height = 8)
      } else {
        x11(width = win.width, height = win.height)
      }
      plot.init.info(cops.init, cops.info)
      # READ FILE ---> cops.raw
      mymessage(paste("      ", "reading", cops.file))
      print("YYYYYY")
      cops.raw <- read.data(cops.file)
      if(verbose) str(cops.raw)
      # CALCULATE DERIVED DATA ---> cops.dd
      cops.dd <- derived.data(info.longitude, info.latitude, cops.init, cops.raw)
      if(cops.dd$change.position) {
        cat("Position has changed (BioGPS.Position column in data file)\n")
        cat("    longitude :", info.longitude, "--->", cops.dd$longitude, "\n")
        cat("    latitude  :", info.latitude , "--->", cops.dd$latitude , "\n")
      }
      if(verbose) str(cops.dd)
      # EVALUATE BLACK ---> cops.black
      cops.black <- evaluate.black(blacks)
      if(verbose) str(cops.black)
      # PROCESS Ed0 ---> cops.Ed0
      cops.Ed0 <- process.Ed0(cops.raw, cops.dd, cops.black)
      if(verbose) str(cops.Ed0)
      # PROCESS LuZ ---> cops.LuZ
      cops.LuZ <- NULL
      if("LuZ" %in% instruments.optics) {
        ###### cops.raw$LuZ = cops.raw$LuZ *-1 ##########ATTENTION
        cops.LuZ <- process.LuZ(cops.raw, cops.dd, cops.black, cops.Ed0)
        if(verbose) str(cops.LuZ)
      }
      # PROCESS EuZ ---> cops.EuZ
      cops.EuZ <- NULL
      if("EuZ" %in% instruments.optics) {
        cops.EuZ <- process.EuZ(cops.raw, cops.dd, cops.black, cops.Ed0)
        if(verbose) str(cops.EuZ)
      }
      # PROCESS EdZ ---> cops.EdZ
      cops.EdZ <- process.EdZ(cops.raw, cops.dd, cops.black, cops.Ed0)
      if(verbose) str(cops.EdZ)

      # Q and f FACTORS
      cops.Qf <- Q.and.f.factors(cops.info, cops.raw, cops.dd, cops.EuZ, cops.LuZ)
      if(verbose) str(cops.Qf)

      # ALL OBJECTS TOGETHER IN (nearly) FINAL list
      cops <- c(cops.init, cops.info, cops.raw, cops.dd,
                cops.black, cops.Ed0, cops.EuZ, cops.LuZ,
                cops.EdZ, cops.Qf)

      # COMPUTE SURFACE AOPs ---> cops.aops
      cops.aops <- compute.aops(cops)
      if(verbose) str(cops.aops)

      # ADD cops.aops TO THE FINAL list
      cops <- c(cops, cops.aops)
      cops$absorption.values <- cops.aops$absorption.values
      if(verbose) str(cops)

      save(file = save.file, cops)

      if (ASCII) dump.asc(asc.filedir, cops)
      # initialize global init parameters for next experiment
      assign("time.window", cops.init00$time.window, env = .GlobalEnv)
      assign("sub.surface.removed.layer.optics", cops.init00$sub.surface.removed.layer.optics, env = .GlobalEnv)
      assign("tiltmax.optics", cops.init00$tiltmax.optics, env = .GlobalEnv)
      assign("time.interval.for.smoothing.optics", cops.init00$time.interval.for.smoothing.optics, env = .GlobalEnv)

      if(!INTERACTIVE) {
        dev.off()
      } else {
        if(experiment != experiments) {
          next.experiment <- tk_messageBox(type = "yesno", "Next experiment")
          if(next.experiment == "no") {
            del.win <- tk_messageBox(type = "yesno", "Delete all windows ?")
            if(del.win == "yes") graphics.off()
            break
          } else {
            del.win <- tk_messageBox(type = "yesno", "Delete all windows ?")
            if(del.win == "yes") graphics.off()
          }
        } else {
          del.win <- tk_messageBox(type = "yesno", "All processed. Delete all windows ?")
          if(del.win == "yes") graphics.off()
        }
      }
      rm(cops.info, info.longitude, info.latitude, chl, SHADOW.CORRECTION, absorption.waves, absorption.values, blacks, cops.raw, cops.dd, cops.black, cops.Ed0, cops.EuZ, cops.LuZ, cops.EdZ, cops.aops, cops, cops.file, save.file, pdf.file)
    }

######## New code for BIOSHADE data
    if (remove.tab[experiment,2] == "2") {
      mymessage(paste("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "blacks"), head = "#", tail = "-")
      mymessage(paste(unlist(info.tab[experiment, ]), collapse = " "), tail = "#")
      # initialization
      cops.init <- cops.init00
      # line of info.file.dat
      # fields 1 to 3
      cops.file <- info.tab[experiment, 1]
      info.longitude <- info.tab[experiment, 2]
      info.latitude <- info.tab[experiment, 3]
      chl <- NA
      SHADOW.CORRECTION <- FALSE
      absorption.values <- NA
      absorption.waves <- NA
      info4 <- NA

      # fields 5 to 8
      info5 <- info.tab[experiment, 5]
      if(info5 != "x") {
        cat("time.window modified in info.cops.dat file", time.window, "---> ")
        time.window <- as.numeric(unlist(strsplit(info5, ",")))
        cops.init$time.window <- time.window
        assign("time.window", time.window, env = .GlobalEnv)
        rm(time.window)
        cat(time.window, "\n")
      }

      info6 <- NA

      info7 <- info.tab[experiment, 7]
      if(info7 != "x") {
        cat("tiltmax.optics modified in info.cops.dat file", tiltmax.optics, "---> ")
        tiltmax.optics <- as.numeric(unlist(strsplit(info7, ",")))
        names(tiltmax.optics) <- instruments.optics
        cops.init$tiltmax.optics <- tiltmax.optics
        assign("tiltmax.optics", tiltmax.optics, env = .GlobalEnv)
        rm(tiltmax.optics)
        cat(tiltmax.optics, "\n")
      }

      info8 <- info.tab[experiment, 8]
      if(info8 != "x") {
        cat("time.interval.for.smoothing.optics modified in info.cops.dat file", time.interval.for.smoothing.optics, "---> ")
        time.interval.for.smoothing.optics <- as.numeric(unlist(strsplit(info8, ",")))
        names(time.interval.for.smoothing.optics) <- instruments.optics
        cops.init$time.interval.for.smoothing.optics <- time.interval.for.smoothing.optics
        assign("time.interval.for.smoothing.optics", time.interval.for.smoothing.optics, env = .GlobalEnv)
        rm(time.interval.for.smoothing.optics)
        cat(time.interval.for.smoothing.optics, "\n")
      }

      if(verbose) str(cops.init)
      # blacks
      blacks <- info.tab[experiment, 9:12]
      blacks <- blacks[blacks != ""]
      cops.info <- list(file = cops.file, chl = chl, SHADOW.CORRECTION = SHADOW.CORRECTION, absorption.waves = absorption.waves, absorption.values = absorption.values, blacks = blacks)
      if(verbose) str(cops.info)

      save.file <- paste(dirres, paste(cops.file, "RData", sep = "."), sep = "/")
      if (ASCII) {
        asc.filedir <- paste(dirasc, paste(cops.file, "asc", sep = "."), sep = "/")
        if(!file.exists(asc.filedir)) dir.create(asc.filedir)
      }
      pdf.file <- paste(dirpdf, paste(cops.file, "pdf", sep = "."), sep = "/")
      if(!INTERACTIVE) {
        pdf(file = pdf.file, width = 11, height = 8)
      } else {
        x11(width = win.width, height = win.height)
      }
      plot.init.info(cops.init, cops.info)
      # READ FILE ---> cops.raw
      mymessage(paste("      ", "reading", cops.file))
      #print("YYYYYY")

      cops.raw <- read.data(cops.file)

      if(verbose) str(cops.raw)
      # CALCULATE DERIVED DATA ---> cops.dd
      cops.dd <- derived.data(info.longitude, info.latitude, cops.init, cops.raw)
      if(cops.dd$change.position) {
        cat("Position has changed (BioGPS.Position column in data file)\n")
        cat("    longitude :", info.longitude, "--->", cops.dd$longitude, "\n")
        cat("    latitude  :", info.latitude , "--->", cops.dd$latitude , "\n")
      }
      if(verbose) str(cops.dd)
      # EVALUATE BLACK ---> cops.black
      cops.black <- evaluate.black(blacks)
      if(verbose) str(cops.black)

      # PROCESS Ed0 ---> cops.Ed0
      cops.Ed0 <- process.Ed0.BioShade(cops.raw, cops.dd, cops.black)
      if(verbose) str(cops.Ed0)



      # ALL OBJECTS TOGETHER IN (nearly) FINAL list
      cops <- c(cops.init, cops.info, cops.raw, cops.dd, cops.black, cops.Ed0)

      if(verbose) str(cops)

      save(file = save.file, cops)

      if (ASCII) dump.asc(asc.filedir, cops)
      # initialize global init parameters for next experiment
      assign("time.window", cops.init00$time.window, env = .GlobalEnv)
      assign("sub.surface.removed.layer.optics", cops.init00$sub.surface.removed.layer.optics, env = .GlobalEnv)
      assign("tiltmax.optics", cops.init00$tiltmax.optics, env = .GlobalEnv)
      assign("time.interval.for.smoothing.optics", cops.init00$time.interval.for.smoothing.optics, env = .GlobalEnv)

      if(!INTERACTIVE) {
        dev.off()
      } else {
        if(experiment != experiments) {
          next.experiment <- tk_messageBox(type = "yesno", "Next experiment")
          if(next.experiment == "no") {
            del.win <- tk_messageBox(type = "yesno", "Delete all windows ?")
            if(del.win == "yes") graphics.off()
            break
          } else {
            del.win <- tk_messageBox(type = "yesno", "Delete all windows ?")
            if(del.win == "yes") graphics.off()
          }
        } else {
          del.win <- tk_messageBox(type = "yesno", "All processed. Delete all windows ?")
          if(del.win == "yes") graphics.off()
        }
      }
      rm(cops.info, info.longitude, info.latitude, chl, SHADOW.CORRECTION, absorption.waves, absorption.values, blacks, cops.raw, cops.dd, cops.black, cops.Ed0, cops.EuZ, cops.LuZ, cops.EdZ, cops.aops, cops, cops.file, save.file, pdf.file)
    }



  }


}
