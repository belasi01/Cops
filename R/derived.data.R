derived.data <- function(lon, lat, cops.init, cops.raw) {
	mymessage(paste("      ", "calculating derived data"))


  if (str_detect(cops.raw$file, "_SB_")) {
    print("Shadow band file format detected")
    print("Change the init parameters")
    number.of.fields.before.date.tmp <- number.of.fields.before.date
    number.of.fields.before.date <- number.of.fields.before.date - 1
    instruments.optics.tmp <- instruments.optics
    instruments.optics <- "Ed0"
  }

	instruments <- c(instruments.optics)
	d2r <- pi / 180
# tilt from Roll and Pitch
	ret <- list()
	for(instr in instruments) {
		instr.anc <- paste(instr, "anc", sep = ".")
		if("Roll" %in% names(cops.raw[[instr.anc]])) {
			Roll <- cops.raw[[instr.anc]]$Roll
			Pitch <- cops.raw[[instr.anc]]$Pitch
			tilt <- atan(sqrt(tan(Roll*d2r)^2+tan(Pitch*d2r)^2))/d2r
		} else {
			tilt <- NULL
		}
		ret <- c(ret, list(tilt))
	}
	names(ret) <- paste(instruments, "tilt", sep = ".")

# dates
	##### Added by Simon Belanger, 31 march 2016
	if (str_detect(cops.raw$Others$DateTime[1], "PM") | str_detect(cops.raw$Others$DateTime[1], "AM")) {
	  cops.raw$Others$DateTime = convert.time.12h.to.24h.string(cops.raw$Others$DateTime, cops.init, 0)
	}
	####
	dates <- as.POSIXct(strptime(cops.raw$Others$DateTime, format = format.date[1]))

	dates.secs.from.beginning <- as.numeric(dates) - min(as.numeric(dates))
	dates.good <- dates.secs.from.beginning >= time.window[1] & dates.secs.from.beginning <= time.window[2]
	cops.duration.secs <- max(as.numeric(dates[dates.good])) - min(as.numeric(dates[dates.good]))
	dte <- mean.POSIXct(dates[dates.good])
 # dte <- dates[dates.good][1]
  print(dates[1])
# longitude and latitude if BioGPS.Position column is present
	if("BioGPS.anc" %in% names(cops.raw)) {
		dummy <- cops.raw[["BioGPS.anc"]]$Position
		position.signs <- sign(dummy)
		dummy <- abs(dummy)
		pos.deg <- floor(dummy / 100)
		pos.min <- dummy - pos.deg * 100
		position <- (pos.deg + pos.min / 60) * position.signs
		i.lon <- abs(position - lon) < 0.5
		i.lat <- abs(position - lat) < 0.5
		if(length(which(i.lon)) < 0.5 | length(which(i.lat)) < 0.5) {
			cat("if BioGPS.Position column is present in data file, you have to indicate\n")
			cat("   approximate (+- 0.5 degree) longitude and latitude in file info.cops.dat\n")
			cat("mismatch between longitude and latitude in file info.cops.dat\n")
			cat("   ", lon, lat, "\n")
			cat("AND    longitude and latitude in data file (BioGPS.Position column)\n")
			hh <- hist(position, plot = FALSE)
			cat("   here is the histogram of the values\n")
			cat("      breaks :", hh$breaks, "\n")
			cat("      counts :", hh$counts, "\n")
			stop()
		}
		longitude <- median(position[i.lon])
		latitude <- median(position[i.lat])
		change.position <- TRUE
	} else {
# if not
		if(is.na(lon) | is.na(lat)) {
# look for a gps file
			gps.files <- paste(dirdat, cops.raw$potential.gps.file, sep = "/")
			if(! file.exists(gps.files)) {
				cat("longitude and(or) latitude not available\n")
				cat(cops.raw$potential.gps.files, "\n")
				cat("3 possibilities :\n")
				cat("- PUT longitude and latitude in file info.cops.dat : 2nd and 3rd column\n")
				cat("- USE BioGPS.Position column if present in data file; in this case,\n")
				cat("      you have to indicate in file info.cops.dat (2nd and 3rd column)\n")
				cat("      approximate (+- 0.5 degree) longitude and latitude\n")
				cat("- PUT NA in file info.cops.dat (2nd and 3rd column)iin this case,\n")
				cat("      a valid GPS file must be present in directory", dirdat,"\n")
				cat("      such a file has a name with the same first fields as the name of the present data file\n")
				cat("      until the time field, then add \"_gps\" and any extension\n")
				stop()
			} else {
# Added by Simon Belanger
			  if (length(gps.files) > 1) {
			    cat("Multiple gps files available\n")
			    xx = unlist(strsplit(cops.raw$file, "_data_"))
			    yy = unlist(strsplit(gps.files, "_gps_"))
			    ix.file = which(yy == xx[2]) / 2
			    gps.file=gps.files[ix.file]
			  } else gps.file=gps.files
#Added by Servet Cizmeli and Simon Belanger
			    print("Reading GPS file: ")
			    print(gps.file)
			    # Modified by Simon Belanger on Aug 2016 to process
			    # GPS data obtained with uprofile 1.9.10 and after
			    if (str_detect(gps.file, "GPS_")) {
			      ext = unlist(strsplit(gps.file, "[.]"))[2]
			      if (ext == "tsv" || ext =="txt") {
			        gps.data <- read.table(gps.file,sep="\t",
			                               colClasses = c("character","character","numeric","character","numeric","numeric","numeric"),header=TRUE)

			      } else {
			        gps.data <- read.table(gps.file,sep=",",
			                               colClasses = c("character","character","numeric","character","numeric","numeric","numeric"),header=TRUE)

			      }
			      names(gps.data)[4] <- "GpsTime"
			      names(gps.data)[1] <- "ComputerTime"
			    } else {
			      ext = unlist(strsplit(gps.file, "[.]"))[2]
			      if (ext == "tsv" || ext =="txt") {
			        gps.data <- read.table(gps.file,sep="\t",
			                               colClasses = c("character","numeric","character","numeric","numeric","numeric"),header=TRUE)
			      } else {
			        gps.data <- read.table(gps.file,sep=",",
			                               colClasses = c("character","numeric","character","numeric","numeric","numeric"),header=TRUE)
			      }
			      names(gps.data)[3] <- "GpsTime"
			      names(gps.data)[1] <- "ComputerTime"
			    }
			    ### END

     			if (str_detect(gps.data$ComputerTime[1], "PM") | str_detect(gps.data$ComputerTime[1], "AM")) {
     			  gps.data$ComputerTime = convert.time.12h.to.24h.string(gps.data$ComputerTime, cops.init, 0)
     			}
     			if (str_detect(gps.data$GpsTime[1], "PM") | str_detect(gps.data$GpsTime[1], "AM")) {
     			  gps.data$GpsTime = convert.time.12h.to.24h.string(gps.data$GpsTime, cops.init, 0)
     			}

			    ### Add by S Belanger in 2018 to deal with GPS having different
			    ### DateTime format than the COPS profiles (in was the case in 2015).
			    ### The user need to provide two format.date in the init.cops.dat file
			    ### separated by coma.
			    if (length(cops.init$format.date)==2) {
			      gps.data$GpsTime <- as.POSIXct(strptime(gps.data$GpsTime,  format = cops.init$format.date[2]))
			      gps.data$ComputerTime <- as.POSIXct(strptime(gps.data$ComputerTime,  format = cops.init$format.date[2]))
			    } else {
			      gps.data$GpsTime <- as.POSIXct(strptime(gps.data$GpsTime,  format = cops.init$format.date[1]))
			      gps.data$ComputerTime <- as.POSIXct(strptime(gps.data$ComputerTime,  format = cops.init$format.date[1]))
			    }
          if (is.na(gps.data$GpsTime[1])) {
            print("WARNING: It seems that the GPS time format differs from the CAST files")
            print("1. Check the time format inside the GPS file")
            print("2. If it differs from the format of the DateTime column of the CAST files,")
            print("   than add a second time format in the init.cops.dat at the  ")
            print("   format.date field (separated by coma).")
            print("  ")
            print(" Teminate processing of that profile.")
            return(0)
          }

     			valid_gps <- gps.data$GpsTime > min(dates) & gps.data$GpsTime < max(dates)
     			if (any(valid_gps)) {
     			  longitude <- median(gps.data$Longitude[valid_gps], na.rm = TRUE)
     			  latitude <- median(gps.data$Latitude[valid_gps], na.rm = TRUE)
     			  change.position <- TRUE
     			} else {
     			  valid_gps <- gps.data$ComputerTime > min(dates) & gps.data$ComputerTime < max(dates)
     			  if (any(valid_gps)) {
     			    longitude <- median(gps.data$Longitude[valid_gps], na.rm = TRUE)
     			    latitude <- median(gps.data$Latitude[valid_gps], na.rm = TRUE)
     			    change.position <- TRUE
     			  } else {
     			    print("Time found in gps file do not match time in")
     			    print(cops.raw$file)
     			  }
     			}

			}
#END - Added by Servet Cizmeli
		} else {
# keep longitude and latitude of file info.cops.dat
			longitude <- lon
			latitude <- lat
			change.position <- FALSE
		}
	}
	cat("longitude :", longitude, "\n")
	cat("latitude :", latitude, "\n")
	print(paste("date: ", dte))

# sun-zenith angle
	day <- as.numeric(format(dte, format = "%d"))
	month <- as.numeric(format(dte, format = "%m"))
	year <- as.numeric(format(dte, format = "%Y"))
	hour <- as.numeric(format(dte, format = "%H"))
	minute <- as.numeric(format(dte, format = "%M"))
	second <- as.numeric(format(dte, format = "%S"))
	ah <- hour + minute / 60 + second / 3600
  print(month)
	sunzen <- possol(month, day, ah, longitude, latitude)[1]

	if (str_detect(cops.raw$file, "_SB_")) {
	  Depth<-NULL
	  Depth.good<-NULL
	  depth.fitted <- NULL
	  ret <- c(ret, list(change.position = change.position,
	                     longitude = longitude,
	                     latitude = latitude,
	                     dates = dates,
	                     date.mean = dte,
	                     cops.duration.secs = cops.duration.secs,
	                     day = day,
	                     month = month,
	                     year = year,
	                     sunzen = sunzen,
	                     Depth = Depth,
	                     Depth.good = Depth.good,
	                     depth.fitted = depth.fitted))
	} else {
	  # ELIMINATE A FEW INVALID DEPTHS
	  Depth <- cops.raw[[paste(depth.is.on, "anc", sep = ".")]]$Depth #+ delta.capteur.optics["LuZ"]
	  # median filter on Depth with big delta to remove a few inappropriate depths
	  delta <- max(Depth) / length(Depth) * 50
	  Depth.filtered <- filtre.mediane(3, Depth, delta = delta, fill = TRUE, replace = FALSE)
	  Depth.good <- !is.na(Depth.filtered)

	  # agglomerate dates.good with Depth.good
	  Depth.good <- Depth.good & dates.good

	  # depth where data will be fitted
	  depth.fitted <- NULL
	  for(i in seq(1, length(depth.discretization) - 2, 2)) {
	    depth.fitted <- append(depth.fitted, seq(depth.discretization[i], depth.discretization[i + 2] - depth.discretization[i + 1] / 2, depth.discretization[i + 1]))
	  }
	  maxdepth <- max(Depth[Depth.good]+ delta.capteur.optics["LuZ"])
	  depth.fitted <- depth.fitted[depth.fitted <= maxdepth]

	  ret <- c(ret, list(change.position = change.position,
	                     longitude = longitude,
	                     latitude = latitude,
	                     dates = dates,
	                     date.mean = dte,
	                     cops.duration.secs = cops.duration.secs,
	                     day = day,
	                     month = month,
	                     year = year,
	                     sunzen = sunzen,
	                     Depth = Depth,
	                     Depth.good = Depth.good,
	                     depth.fitted = depth.fitted))
	  # PLOT
	  if(INTERACTIVE) x11(width = win.width, height = win.height)
	  plot(dates, Depth, ylim = rev(range(Depth)), xlab = "UTC Time (HH:MM:SS)", ylab = "Depth (m)", main = paste(dte, "   duration =", cops.duration.secs, "s", "   lon =", round(longitude, digits = 3), "   lat =", round(latitude, digits = 3), "   sun-zenith angle =", round(sunzen, digits = 2), "deg."), cex.main = 1, axes = FALSE, frame.plot = TRUE)
	  grid(col = 1)
	  axis.POSIXct(1, dates, format = "%H:%M:%S")
	  axis(2)
	  points(dates[!Depth.good], Depth[!Depth.good], pch = 20, col = 2, cex = 2)
	  legend("bottomleft", legend = paste(length(which(!Depth.good)), "points removed"), text.col = 2, cex = 2)
	  if(change.position) legend("topright", legend = c("Longitude and Latitute found", "   (BioGPS column present)", "VALUES MODIFIED"), text.col = 3, cex = 1)
	  if(INTERACTIVE) x11(width = win.width, height = win.height)
	  par(mfrow = c(1, length(instruments)))
	  for(instr in instruments) {
	    tilt <- ret[[paste(instr, "tilt", sep = ".")]]
	    tiltmax <- tiltmax.optics[instr]
	    if(!is.null(tilt)) {
	      plot(tilt[Depth.good], Depth[Depth.good],
	           ylim = rev(range(Depth)),
	           xlab = "Tilt (degree)", ylab = "Depth (m)", main = instr)
	      abline(v=tiltmax.optics[instr], col=2, lwd=2)
	      grid(col = 1)
	      points(tilt[!Depth.good], Depth[!Depth.good], col = 8) # grey
	      points(tilt[tilt > tiltmax], Depth[tilt > tiltmax], col = 2) # red
	    }
	  }
	  par(mfrow = c(1, 1))

	}





	ret
}
