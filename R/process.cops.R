#'  This the main function of the Cops package and is
#'  called by \code{\link{cops.go}}
#'
#'@param dirdat is the working directory string
#' @param ASCII is a logical value: if TRUE the outputs are written in simple ASCII files in a ddition to
#' the binary files in RData format.
#' @param CLEAN.FILES is a logical value: if TRUE, the user will be prompt
#' to select the good part of the COPS file interactively.
#' IMPORTANT: the input cops file will be OVERWRITE.
#'
#' @author Bernard Gentili and Simon Belanger
#' @export
process.cops <- function(dirdat, ASCII=FALSE, CLEAN.FILES=FALSE) {
	# initialisation file
	generic.init.file <- paste( Sys.getenv("R_COPS_DATA_DIR"), "init.parameters.dat", sep = "/")

	init.file <- paste(dirdat, "init.cops.dat", sep = "/")
	if(!file.exists(init.file)) {
		file.copy(from = generic.init.file, to = init.file)
		cat("EDIT file", init.file, "and CUSTOMIZE IT\n")
		stop("Please edit init file and run again")
	}
	cops.init00 <- read.init(init.file)

	# Check for the parameter use to fit with loess
	# If init.file.dat contains time.interval.for.smoothing.optics,
	# then convert it for depth.interval.for.smoothing.optics
	# this is done later, after the call to derived.data()
	if (is.null(cops.init00$depth.interval.for.smoothing.optics)) {
	  # interval.for.smoothing.optics is provided in terms of TIME
	  DEPTH.SPAN = FALSE
	} else {
	  DEPTH.SPAN = TRUE
	}

	# information file
	header.info.file <- paste(Sys.getenv("R_COPS_DATA_DIR"), "info.header.dat", sep = "/")
	info.file <- paste(dirdat, "info.cops.dat", sep = "/")
	if(!file.exists(info.file)) {
		file.copy(from = header.info.file, to = info.file)
		files.in.dirdat <- list.files(dirdat)
		files.in.dirdat <- files.in.dirdat[! files.in.dirdat %in% c("init.cops.dat", "info.cops.dat")]
		lines.in.info.file <- paste(files.in.dirdat, "NA", "NA", "999", "x", "x", "x","x","x","x", sep = ";")
		write(file = info.file, lines.in.info.file, append = TRUE, ncolumns = 1)
		cat("EDIT file", info.file, "and CUSTOMIZE IT\n")
		cat("  this file must contain as much lines as cops-experiments you want to process\n")
		cat("  you will find a header with instructions to fill this file\n")
		stop()
	}
	info.tab <- read.table(info.file,
		colClasses = c(
			"character", "numeric", "numeric", "numeric",
			"character", "character", "character", "character","character","character",
			"character", "character", "character", "character"),
		col.names = c("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "r2thresh", "maxdelta" , "b1", "b2", "b3", "b4"),
		header = FALSE, fill = TRUE, sep = ";")

	# removal file is obsolete. Version >4.0 uses select.cops.dat
	# If remove.file.dat is present instead of select.cops.dat,
	# then rename and edit the file
	remove.file <- paste(dirdat, "remove.cops.dat", sep = "/")
	select.file <- paste(dirdat, "select.cops.dat", sep = "/")
	if(!file.exists(remove.file) && !file.exists(select.file)) {
	  # none of them exist
		write.table(file = select.file, cbind(info.tab[, 1], "1", "Rrs.0p.linear", "0"), col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
	}
	if (file.exists(remove.file) && !file.exists(select.file)) {
	  # only remove.file exist
	  print("remove.file.dat is obsolete => Changing to select.cops.dat")
	  remove.tab <- read.table(remove.file, header = FALSE, colClasses = "character", sep = ";")
	  select.tab <- cbind(remove.tab, "Rrs.0p.linear", "0")
	  write.table(file = select.file, select.tab, col.names = FALSE, row.names = FALSE, quote = FALSE, sep = ";")
	}
	select.tab <- read.table(select.file,
	                         header = FALSE,
	                         colClasses = "character",
	                         sep = ";")
	if(!isTRUE(all.equal(select.tab[[1]], info.tab[[1]]))) stop("info.cops.dat select.cops.dat non compatibles")

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

	# find the type of files
	kept.cast <- select.tab[[2]] == "1" | select.tab[[2]] == "3"
	kept.bioS <- select.tab[[2]] == "2"

	kept <- kept.cast | kept.bioS
	cat("discarded experiments", info.tab[!kept, 1], "\n")
	cat("processed Profile experiments", info.tab[kept.cast, 1], "\n")
	cat("processed BioShade experiments", info.tab[kept.bioS, 1], "\n")
	info.tab <- info.tab[kept, ]
	select.tab <- select.tab[kept, ]

	#### order the file to process to allow Shadow Band processing first
	ix <- sort.int(as.numeric(select.tab$V2), decreasing = T, index.return = T)$ix
	select.tab<-select.tab[ix,]
	info.tab<-info.tab[ix,]

	assign("dirres", dirres, env = .GlobalEnv)
	assign("dirdat", dirdat, env = .GlobalEnv)
	assign("dirpdf", dirpdf, env = .GlobalEnv)
	assign("info.file", info.file,  env = .GlobalEnv)
	assign("remove.file", remove.file,  env = .GlobalEnv)
	assign("info.tab", info.tab,  env = .GlobalEnv)
	assign("select.tab", select.tab,  env = .GlobalEnv)

	if(all(!kept)) {
		cat("NOTHING TO DO\n")
		stop()
	}
	experiments <- nrow(info.tab)
	for(experiment in 1:experiments) {

    if (select.tab[experiment,2] == "1"  | select.tab[experiment,2] == "3" ) {
      mymessage(paste("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo",  "r2thresh", "maxdelta" ,"blacks"), head = "#", tail = "-")
      mymessage(paste(unlist(info.tab[experiment, ]), collapse = " "), tail = "#")

      if (select.tab[experiment,2] == "3" ) {
        print("NO extrapolation at 0- for that profile.")
        EXTRAPOLATION.0m = FALSE
      } else {
        EXTRAPOLATION.0m = TRUE
      }

      if (!is.na(select.tab[experiment,4]) & select.tab[experiment,4] == "1" ) {
        print("Shallow water. Profile finished just above the bottom")
        SHALLOW = TRUE
      } else {
        SHALLOW = FALSE
      }

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
      if(!is.na(chl) && EXTRAPOLATION.0m) {
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
      # fields 5 to 8 # Time window
      info5 <- info.tab[experiment, 5]

      # Added by Simon Bélanger in april 2020 to clean shallow water profile
      # interactively
      if (CLEAN.FILES) {
        print("Clean file interactively")
        if(info5 == "x") {
          print("WARNING: the time.window parameter of the file.info.dat")
          print("will be reset to x to take all measurements.")
          info5 <- "x"
        }
        clean.cops.file(cops.file, out.file = NA)
      }

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
        if (DEPTH.SPAN) {
          cat("depth.interval.for.smoothing.optics modified in info.cops.dat file", depth.interval.for.smoothing.optics, "---> ")
          depth.interval.for.smoothing.optics <- as.numeric(unlist(strsplit(info8, ",")))
          names(depth.interval.for.smoothing.optics) <- instruments.optics
          cops.init$depth.interval.for.smoothing.optics <- depth.interval.for.smoothing.optics
          assign("depth.interval.for.smoothing.optics", depth.interval.for.smoothing.optics, env = .GlobalEnv)
          rm(depth.interval.for.smoothing.optics)
          cat(depth.interval.for.smoothing.optics, "\n")

        } else {
          cat("time.interval.for.smoothing.optics modified in info.cops.dat file", time.interval.for.smoothing.optics, "---> ")
          time.interval.for.smoothing.optics <- as.numeric(unlist(strsplit(info8, ",")))
          names(time.interval.for.smoothing.optics) <- instruments.optics
          cops.init$time.interval.for.smoothing.optics <- time.interval.for.smoothing.optics
          assign("time.interval.for.smoothing.optics", time.interval.for.smoothing.optics, env = .GlobalEnv)
          rm(time.interval.for.smoothing.optics)
          cat(time.interval.for.smoothing.optics, "\n")
        }
      }
      #### Added new parameters for linear fitting
      info9 <- info.tab[experiment, 9]
      if(info9 != "x") {
        cat("linear.fit.Rsquared.threshold.optics modified in info.cops.dat file", linear.fit.Rsquared.threshold.optics, "---> ")
        linear.fit.Rsquared.threshold.optics <- as.numeric(unlist(strsplit(info9, ",")))
        names(linear.fit.Rsquared.threshold.optics) <- instruments.optics
        cops.init$linear.fit.Rsquared.threshold.optics <- linear.fit.Rsquared.threshold.optics
        assign("linear.fit.Rsquared.threshold.optics", linear.fit.Rsquared.threshold.optics, env = .GlobalEnv)
        rm(linear.fit.Rsquared.threshold.optics)
        cat(linear.fit.Rsquared.threshold.optics, "\n")
      }
      info10 <- info.tab[experiment, 10]
      if(info10 != "x") {
        cat("linear.fit.max.delta.depth.optics modified in info.cops.dat file", linear.fit.max.delta.depth.optics, "---> ")
        linear.fit.max.delta.depth.optics <- as.numeric(unlist(strsplit(info10, ",")))
        names(linear.fit.max.delta.depth.optics) <- instruments.optics
        cops.init$linear.fit.max.delta.depth.optics <- linear.fit.max.delta.depth.optics
        assign("linear.fit.max.delta.depth.optics", linear.fit.max.delta.depth.optics, env = .GlobalEnv)
        rm(linear.fit.max.delta.depth.optics)
        cat(linear.fit.max.delta.depth.optics, "\n")
      }
      #####

      if(verbose) str(cops.init)
      # blacks
      blacks <- info.tab[experiment, 11:14]
      blacks <- blacks[blacks != ""]
      cops.info <- list(file = cops.file,
                        chl = chl,
                        EXTRAPOLATION.0m = EXTRAPOLATION.0m,
                        SHALLOW = SHALLOW,
                        SHADOW.CORRECTION = SHADOW.CORRECTION,
                        absorption.waves = absorption.waves,
                        absorption.values = absorption.values,
                        blacks = blacks)
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

      # Define an instrument detection limit variables
      # (<<- assign it as .Global variable)
      for (instrument in instruments.optics) {

        if (instrument == "EdZ") {EdZ.detect.lim <<- spline(detection.limit$waves,
                                                           detection.limit[,2],
                                                           xout = cops.raw$EdZ.waves)$y
        print("EdZ detection limit is: ")
        print(data.frame(waves=cops.raw$EdZ.waves,
                         detection.limit=EdZ.detect.lim))
        assign("EdZ.detect.lim", EdZ.detect.lim,envir = .GlobalEnv)
        rm(EdZ.detect.lim)}

        if (instrument == "EuZ") {EuZ.detect.lim <<- spline(detection.limit$waves,
                                                           detection.limit[,3],
                                                           xout = cops.raw$EuZ.waves)$y
        print("EuZ detection limit is: ")
        print(data.frame(waves=cops.raw$EuZ.waves,
                         detection.limit=EuZ.detect.lim))
        assign("EuZ.detect.lim", EuZ.detect.lim, envir = .GlobalEnv)
        rm(EuZ.detect.lim)}

        if (instrument == "LuZ") {LuZ.detect.lim <<- spline(detection.limit$waves,
                                                           detection.limit[,4],
                                                           xout = cops.raw$LuZ.waves)$y
        print("LuZ detection limit is: ")
        print(data.frame(waves=cops.raw$LuZ.waves,
                         detection.limit=LuZ.detect.lim))
        assign("LuZ.detect.lim", LuZ.detect.lim,envir = .GlobalEnv)
        rm(LuZ.detect.lim)
        cat(LuZ.detect.lim, "\n")}
      }

      # CALCULATE DERIVED DATA ---> cops.dd
      cops.dd <- derived.data(info.longitude, info.latitude, cops.init, cops.raw)

      #
      if (!DEPTH.SPAN) {
        print("Converting time.interval.for.smoothing.optics into depth.interval.for.smoothing.optics")
        depth.interval.for.smoothing.optics <- time.interval.for.smoothing.optics/cops.dd$cops.duration.secs* max(cops.dd$depth.fitted)
        print(paste("time.interval : ", time.interval.for.smoothing.optics))
        print(paste("=> depth.interval : ", depth.interval.for.smoothing.optics))
        cops.init$depth.interval.for.smoothing.optics <- depth.interval.for.smoothing.optics
        assign("depth.interval.for.smoothing.optics", depth.interval.for.smoothing.optics, env = .GlobalEnv)
        rm(depth.interval.for.smoothing.optics)
        cat(depth.interval.for.smoothing.optics, "\n")
      }

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
         cops.LuZ <- process.LuZ(cops.raw, cops.dd, cops.black, cops.Ed0, EXTRAPOLATION.0m)
        if(verbose) str(cops.LuZ)
      }
      # PROCESS EuZ ---> cops.EuZ
      cops.EuZ <- NULL
      if("EuZ" %in% instruments.optics) {
        cops.EuZ <- process.EuZ(cops.raw, cops.dd, cops.black, cops.Ed0, EXTRAPOLATION.0m)
        if(verbose) str(cops.EuZ)
      }
      # PROCESS EdZ ---> cops.EdZ
      cops.EdZ <- process.EdZ(cops.raw, cops.dd, cops.black, cops.Ed0, EXTRAPOLATION.0m)
      if(verbose) str(cops.EdZ)

      # Q and f FACTORS
      cops.Qf <- Q.and.f.factors(cops.info, cops.raw, cops.dd, cops.EuZ, cops.LuZ)
      if(verbose) str(cops.Qf)

      # ALL OBJECTS TOGETHER IN (nearly) FINAL list
      cops <- c(cops.init, cops.info, cops.raw, cops.dd,
                cops.black, cops.Ed0, cops.EuZ, cops.LuZ,
                cops.EdZ, cops.Qf)

      # added by Guislain - February 2017
      # PARd and PARu from measured and fitted values of EdZ and EuZ
      cops.PAR.fitted <- compute.PAR.fitted(cops)

      # Update the list
      cops   <- c(cops, cops.PAR.fitted) # c(cops, cops.PAR, cops.PAR.fitted)


      # COMPUTE SURFACE AOPs ---> cops.aops
      #if (all(is.na(cops$)))
      cops.aops <- compute.aops(cops)
      if(verbose) str(cops.aops)

      # ADD cops.aops to the list
      cops <- c(cops, cops.aops)

      #  Compute bottom properties if SHALLOW
      if (SHALLOW) {
          cops.shallow <- compute.bottom(cops)
          cops <- c(cops, cops.shallow)
      }


      cops$absorption.values <- cops.aops$absorption.values
      #if(verbose) str(cops)

      save(file = save.file, cops)

      if (ASCII) dump.asc(asc.filedir, cops)
      # initialize global init parameters for next experiment
      assign("time.window", cops.init00$time.window, env = .GlobalEnv)
      assign("sub.surface.removed.layer.optics", cops.init00$sub.surface.removed.layer.optics, env = .GlobalEnv)
      assign("tiltmax.optics", cops.init00$tiltmax.optics, env = .GlobalEnv)
###     # assign("tiltmax.optics", cops.init00$tiltmax.optics, env = .GlobalEnv)

      if (DEPTH.SPAN) {
        assign("depth.interval.for.smoothing.optics", cops.init00$depth.interval.for.smoothing.optics, env = .GlobalEnv)
      } else {
        assign("time.interval.for.smoothing.optics", cops.init00$time.interval.for.smoothing.optics, env = .GlobalEnv)
      }

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
    if (select.tab[experiment,2] == "2") {
      mymessage(paste("file", "lon", "lat", "chl", "timwin", "ssrm", "tiltm", "smoo", "r2thresh", "maxdelta" ,"blacks"), head = "#", tail = "-")
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
        if (DEPTH.SPAN) {
          cat("depth.interval.for.smoothing.optics modified in info.cops.dat file", depth.interval.for.smoothing.optics, "---> ")
          depth.interval.for.smoothing.optics <- as.numeric(unlist(strsplit(info8, ",")))
          names(depth.interval.for.smoothing.optics) <- instruments.optics
          cops.init$depth.interval.for.smoothing.optics <- depth.interval.for.smoothing.optics
          assign("depth.interval.for.smoothing.optics", depth.interval.for.smoothing.optics, env = .GlobalEnv)
          rm(depth.interval.for.smoothing.optics)
          cat(depth.interval.for.smoothing.optics, "\n")

        } else {
          cat("time.interval.for.smoothing.optics modified in info.cops.dat file", time.interval.for.smoothing.optics, "---> ")
          time.interval.for.smoothing.optics <- as.numeric(unlist(strsplit(info8, ",")))
          names(time.interval.for.smoothing.optics) <- instruments.optics
          cops.init$time.interval.for.smoothing.optics <- time.interval.for.smoothing.optics
          assign("time.interval.for.smoothing.optics", time.interval.for.smoothing.optics, env = .GlobalEnv)
          rm(time.interval.for.smoothing.optics)
          cat(time.interval.for.smoothing.optics, "\n")
        }
      }
      #### Added new parameters for linear fitting
      info9 <- info.tab[experiment, 9]
      if(info9 != "x") {
        cat("linear.fit.Rsquared.threshold.optics modified in info.cops.dat file", linear.fit.Rsquared.threshold.optics, "---> ")
        linear.fit.Rsquared.threshold.optics <- as.numeric(unlist(strsplit(info9, ",")))
        names(linear.fit.Rsquared.threshold.optics) <- instruments.optics
        cops.init$linear.fit.Rsquared.threshold.optics <- linear.fit.Rsquared.threshold.optics
        assign("linear.fit.Rsquared.threshold.optics", linear.fit.Rsquared.threshold.optics, env = .GlobalEnv)
        rm(linear.fit.Rsquared.threshold.optics)
        cat(linear.fit.Rsquared.threshold.optics, "\n")
      }
      info10 <- info.tab[experiment, 10]
      if(info10 != "x") {
        cat("linear.fit.max.delta.depth.optics modified in info.cops.dat file", linear.fit.max.delta.depth.optics, "---> ")
        linear.fit.max.delta.depth.optics <- as.numeric(unlist(strsplit(info10, ",")))
        names(linear.fit.max.delta.depth.optics) <- instruments.optics
        cops.init$linear.fit.max.delta.depth.optics <- linear.fit.max.delta.depth.optics
        assign("linear.fit.max.delta.depth.optics", linear.fit.max.delta.depth.optics, env = .GlobalEnv)
        rm(linear.fit.max.delta.depth.optics)
        cat(linear.fit.max.delta.depth.optics, "\n")
      }
      #####

      if(verbose) str(cops.init)
      # blacks
      blacks <- info.tab[experiment, 11:14]
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
      if (DEPTH.SPAN) {
        assign("depth.interval.for.smoothing.optics", cops.init00$depth.interval.for.smoothing.optics, env = .GlobalEnv)
      } else {
        assign("time.interval.for.smoothing.optics", cops.init00$time.interval.for.smoothing.optics, env = .GlobalEnv)
      }

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
