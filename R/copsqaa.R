copsqaa <- function(Rrs, idx, dir) {

	filein <- paste(dir, "shift.cops", "qaa.in", sep = "/")
	fileout <- paste(dir, "shift.cops", "qaa.out", sep = "/")
	unlink(filein)
	unlink(fileout)

	waves <- as.numeric(colnames(Rrs))
	experiments <- rownames(Rrs)

#	idx <- c(idx410, idx440, idx490, idx555, idx670)
#	cdx <- c("idx410", "idx440", "idx490", "idx555", "idx670")
	ddx <- data.frame(waves[idx])
#	rownames(ddx) <- cdx
	rownames(ddx) <- names(idx)
	colnames(ddx) <- "wavelengths"
	print(ddx)

	waterfile <- paste(Sys.getenv("R_COPS_DATA_DIR"), "water_spectra.nc", sep = "/")
	nc <-  nc_open(waterfile)
	nc
	aw.nc <- ncvar_get(nc, "aw")
	bw.nc <- ncvar_get(nc, "bw")
	w.nc <- ncvar_get(nc, "wavelength")
	aw <- approx(w.nc, aw.nc, waves)$y
	bbw <- approx(w.nc, bw.nc, waves)$y / 2

	write(file = filein, c(nrow(Rrs), ncol(Rrs)), ncolumns = 2)
	write(file = filein, append = TRUE, waves, ncolumns = length(waves))
	write(file = filein, append = TRUE, aw, ncolumns = length(waves))
	write(file = filein, append = TRUE, bbw, ncolumns = length(waves))
	# idx - 1 because indexes start at 0 in "C" and 1 in "R"
	write(file = filein, append = TRUE, idx - 1, ncolumns = length(waves))
	write(file = filein, append = TRUE, experiments, ncolumns = 1)
	write.table(file = filein, append = TRUE, Rrs, col.names = FALSE, row.names = FALSE)

	.C("qaa", as.character(filein), as.character(fileout))

	return(filein)
}
