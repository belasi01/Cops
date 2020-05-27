copsdata <- function(on.waves.dir, wavemin, wavemax) {
	some.params <- function(f) {
		g <- paste(dirname(f), "params.txt", sep = "/")
		params <- read.table(file = g, header = FALSE, sep = "\t",
			col.names = c("V1", "V2", "V3"), colClasses = rep("character", 3), fill = TRUE)
		rn <- params[[1]]
		params <- params[2:3]
		i.file <- which(rn == "file")
		if(length(i.file == 2)) {
			params <- params[-i.file[2], ]
			rn <- rn[-i.file[2]]
		}
		rownames(params) <- rn
		longitude <- as.numeric(params["longitude", 1])
		latitude <- as.numeric(params["latitude", 1])
		year <- as.numeric(params["year", 1])
		month <- as.numeric(params["month", 1])
		day <- as.numeric(params["day", 1])
		sunzen <- as.numeric(params["sunzen", 1])
		chl <- as.numeric(params["chl", 1])
		return(c(longitude = longitude, latitude = latitude, year = year, month = month, day = day, sunzen = sunzen, chl = chl))
	}
	fs <- list.files(on.waves.dir, pattern = "^on\\.waves\\.txt$", recursive = TRUE, full.names = TRUE)
	begin <- TRUE
	for(f in fs) {
		cat(f, "\n")
		params <- some.params(f)
		x <- read.table(file = f, header = TRUE)
		# Ed0.waves EdZ.waves EuZ.waves Ed0.th Ed0.0p EuZ.0m EdZ.0m Q.0 Q.sun.nadir f.0 f.sun
		# EuZ.shad.aR EuZ.shad.Edif EuZ.shad.Edir EuZ.shad.ratio.edsky.edsun EuZ.shad.eps.sun
		# EuZ.shad.eps.sky EuZ.shad.eps EuZ.shad.correction Lw.0p nLw.0p R.0m Rrs.0p 
		ws <- x$Ed0.waves
		r <- x$Rrs.0p
		es <- x$Ed0.0p
		lw <- x$Lw.0p
		nlw <- x$nLw.0p
		if(begin) {
			Params <- params
			waves <- ws
			Rrs <- r
			Ed0 <- es
			Lw <- lw
			nLw <- nlw
			begin <- FALSE
		} else {
			if(!isTRUE(all.equal(waves, ws))) {
				cat("wavelengths differ\n")
				cat(waves, "\n")
				cat(ws, "\n")
				stop()
			}
			Params <- rbind(Params, params)
			Rrs <- rbind(Rrs, r)
			Ed0 <- rbind(Ed0, es)
			Lw <- rbind(Lw, lw)
			nLw <- rbind(nLw, nlw)
		}
	}
	if(!is.array(Rrs)) {
		Rrs <- array(Rrs, dim = c(1, length(Rrs)))
		Ed0 <- array(Ed0, dim = c(1, length(Ed0)))
		Lw <- array(Lw, dim = c(1, length(Lw)))
		nLw <- array(nLw, dim = c(1, length(nLw)))
	}
	colnames(Rrs) <- waves
	colnames(Ed0) <- waves
	colnames(Lw) <- waves
	colnames(nLw) <- waves
	experiments <- dirname(fs)
	rownames(Params) <- experiments
	rownames(Rrs) <- experiments
	rownames(Ed0) <- experiments
	rownames(Lw) <- experiments
	rownames(nLw) <- experiments
	return(list(params = Params, Rrs = Rrs, Ed0 = Ed0, Lw = Lw, nLw = nLw))
}
