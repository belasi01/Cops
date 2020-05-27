copsshift <- function(cops, new.waves0, nearest, window, dir) {
	qaa.outfile <- paste(dir, "shift.cops", "qaa.out", sep = "/")
	shift.pdffile <- paste(dir, "shift.cops", "shift.pdf", sep = "/")
	################################################################
	old.waves <- scan(file = qaa.outfile, what = 0, skip = 0, nlines = 1, quiet = TRUE)
	iwref <- scan(file = qaa.outfile, what = 0, skip = 1, nlines = 1, quiet = TRUE)
	################################################################
	for(iw in 1:length(new.waves0)) {
		cat(iw, new.waves0[iw], ":", old.waves[nearest[[iw]]], "\n")
	}
	new.waves <- NULL
	nearest.cops.waves.index <- NULL
	for(iw in 1:length(new.waves0)) {
		io <- nearest[[iw]]
		for(i in io) {
#			cat(new.waves0[iw], i, "\n")
			new.waves <- append(new.waves, new.waves0[iw])
			nearest.cops.waves.index <- append(nearest.cops.waves.index, i)
		}
	}
	################################################################
	water <- waterspectra()
	################################################################
	# table A B
	wab <- ABphy()
	################################################################
	dum <- read.table(file = qaa.outfile, header = FALSE, skip = 2, nrows = 2, row.names = 1, as.is = TRUE)
	NUM_SPECTRA <- dum["NUM_SPECTRA", 1]
	NUM_BANDS <- dum["NUM_BANDS", 1]
	cat("\nnumber of spectra", NUM_SPECTRA, "number of bands", NUM_BANDS, "\n\n")
	skipspec <- 4
	pdf(file = shift.pdffile, width = 8, height = 11)
	par(mfrow = c(4, 2))
	par(mai = par("mai") * c(0.5,1,1,1))
	specnames <- rownames(cops$Ed0)
	for(numspec in 1:NUM_SPECTRA) {
		Ed0.old.waves <-  cops$Ed0[numspec, ]
		Lw.old.waves <- cops$Lw[numspec, ]
		nLw.old.waves <- cops$nLw[numspec, ]
		params <- cops$params[numspec, ]
		specname <- specnames[numspec]
		cat("###", specname, "###\n")
		print(params)
		dum <- scan(file = qaa.outfile, what = 0, skip = 1 + skipspec, nlines = 1, quiet = TRUE)
		g0 <- dum[1]
		g1 <- dum[2]
		eta <- dum[3]
		S <- dum[4]
		cat("--->  eta :", eta, "   S :", S, "\n")
		x <- as.data.frame(t(read.table(file = qaa.outfile, header = FALSE, skip = 2 + skipspec, nrows = 10, row.names = 1, as.is = TRUE)))
		skipspec <- skipspec + 12

#		u <- x$bb / (x$a + x$bb)
#		rrs <- u * (g0 + g1 * u)
#		Rrs <- 0.52 * rrs / (1 - 1.7 * rrs)
#		errabs.rrs <- abs(rrs - x$rrs)
#		errabs.Rrs <- abs(Rrs - x$Rrs)
#		errabs <- data.frame(rrs = errabs.rrs, Rrs = errabs.Rrs)
#		cat("Absolute error on rrs and Rrs calculated by QAA inversion\n")
#		print(summary(errabs))

		w.ref <- x$lam[iwref]
		aph.ref <- x$aph[iwref]
		adg.ref <- x$adg[iwref]
		bbp.ref <- x$bb[iwref] - x$bbw[iwref]
		A.ref <- approx(wab[[1]], wab[[2]], w.ref, rule = 2)$y
		B.ref <- approx(wab[[1]], wab[[3]], w.ref, rule = 2)$y

		Rrs.old.waves <- NULL
		for(wt in old.waves) {
			A.wt <- approx(wab[[1]], wab[[2]], wt, rule = 2)$y
			B.wt <- approx(wab[[1]], wab[[3]], wt, rule = 2)$y
			aph.wt <- A.wt * (aph.ref / A.ref)**((1 - B.wt) / (1 - B.ref))
			adg.wt <- adg.ref * exp(-S * (wt - w.ref))
			bbp.wt <- bbp.ref * (w.ref / wt)**eta
			aw.wt <- approx(water$w, water$aw, wt)$y
			bbw.wt <- approx(water$w, water$bw, wt)$y / 2
			bb.wt <- bbp.wt + bbw.wt
			a.wt <- aph.wt + adg.wt + aw.wt
			u.wt <- bb.wt / (a.wt + bb.wt)
			rrs.wt <- u.wt * (g0 + g1 * u.wt)
			Rrs.wt <- 0.52 * rrs.wt / (1 - 1.7 * rrs.wt)
			Rrs.old.waves <- append(Rrs.old.waves, Rrs.wt)
		}

		Rrs.new.waves <- NULL
		for(wt in new.waves) {
			A.wt <- approx(wab[[1]], wab[[2]], wt, rule = 2)$y
			B.wt <- approx(wab[[1]], wab[[3]], wt, rule = 2)$y
			aph.wt <- A.wt * (aph.ref / A.ref)**((1 - B.wt) / (1 - B.ref))
			adg.wt <- adg.ref * exp(-S * (wt - w.ref))
			bbp.wt <- bbp.ref * (w.ref / wt)**eta
			aw.wt <- approx(water$w, water$aw, wt)$y
			bbw.wt <- approx(water$w, water$bw, wt)$y / 2
			bb.wt <- bbp.wt + bbw.wt
			a.wt <- aph.wt + adg.wt + aw.wt
			u.wt <- bb.wt / (a.wt + bb.wt)
			rrs.wt <- u.wt * (g0 + g1 * u.wt)
			Rrs.wt <- 0.52 * rrs.wt / (1 - 1.7 * rrs.wt)
			Rrs.new.waves <- append(Rrs.new.waves, Rrs.wt)
		}
		i.wt <- 0
		for(wt in new.waves) {
			i.wt <- i.wt + 1
			Rrs.new.waves[i.wt] <- Rrs.new.waves[i.wt] * x$Rrs[nearest.cops.waves.index[i.wt]] /
				Rrs.old.waves[nearest.cops.waves.index[i.wt]]
		}

		Ed0.th.old.waves <- func.Ed0.th(params["sunzen"], params["month"], params["day"], old.waves, window)
		Ed0.th.new.waves <- func.Ed0.th(params["sunzen"], params["month"], params["day"], new.waves, window)

		new.waves2 <- NULL
		Rrs.new.waves2 <- NULL
		Ed0.new.waves2 <- NULL
		i.dup.wt <- which(duplicated(new.waves)) - 1
		i.wt <- 0
		while(i.wt < length(new.waves) - 0.5) {
			i.wt <- i.wt + 1
			if(i.wt %in% i.dup.wt) {
				r1 <- Rrs.new.waves[i.wt]
				l1 <- old.waves[nearest.cops.waves.index[i.wt]]
				r2 <- Rrs.new.waves[i.wt + 1]
				l2 <- old.waves[nearest.cops.waves.index[i.wt + 1]]
				l0 <- new.waves[i.wt]
				r12 <- (r1 * (l2 - l0) + r2 * (l0 - l1)) / (l2 - l1)
				l12 <- l0
				new.waves2 <- append(new.waves2, l12)
				Rrs.new.waves2 <- append(Rrs.new.waves2, r12)
				e1 <- Ed0.old.waves[nearest.cops.waves.index[i.wt]] *
					Ed0.th.new.waves[i.wt] /
					Ed0.th.old.waves[nearest.cops.waves.index[i.wt]]
				e2 <- Ed0.old.waves[nearest.cops.waves.index[i.wt + 1]] *
					Ed0.th.new.waves[i.wt + 1] /
					Ed0.th.old.waves[nearest.cops.waves.index[i.wt + 1]]
				e12 <- (e1 * (l2 - l0) + e2 * (l0 - l1)) / (l2 - l1)
				Ed0.new.waves2 <- append(Ed0.new.waves2, e12)
				i.wt <- i.wt + 1
			} else {
				new.waves2 <- append(new.waves2, new.waves[i.wt])
				Rrs.new.waves2 <- append(Rrs.new.waves2, Rrs.new.waves[i.wt])
				e1 <- Ed0.old.waves[nearest.cops.waves.index[i.wt]] *
					Ed0.th.new.waves[i.wt] /
					Ed0.th.old.waves[nearest.cops.waves.index[i.wt]]
				Ed0.new.waves2 <- append(Ed0.new.waves2, e1)
			}
		}
	
		Lw.new.waves2 <- Rrs.new.waves2 * Ed0.new.waves2
		Ed0.th.new.waves2 <- func.Ed0.th(params["sunzen"], params["month"], params["day"], new.waves2, window)
		nLw.new.waves2 <- Rrs.new.waves2 * etirrwindow(new.waves2, window)
		Q.and.f <- Q.and.f.factors2(new.waves2, params["chl"], params["sunzen"])
		# $Q.0 $Q.sun.nadir $f.0 $f.sun

		outfile <-paste(specname, "on.newwaves.txt", sep = "/")
		all.ws <- c(x$lam, new.waves2)
		all.Rrs <- c(x$Rrs, Rrs.new.waves2)
		all.Ed0 <- c(Ed0.old.waves, Ed0.new.waves2)
		all.Lw <- c(Lw.old.waves, Lw.new.waves2)
		all.nLw <- c(nLw.old.waves, nLw.new.waves2)
		all.type <- c(rep("D", length(x$lam)), rep("S", length(new.waves2)))
		all.o <- order(all.ws)
		all.ws <- all.ws[all.o]
		all.Rrs <- all.Rrs[all.o]
		all.Ed0 <- all.Ed0[all.o]
		all.Lw <- all.Lw[all.o]
		all.nLw <- all.nLw[all.o]
		all.type <- all.type[all.o]

		plot(all.ws, all.Rrs, type = "l", xlab = "wavelength", ylab = expression(R[rs]),
			main = c(dirname(specname), basename(specname)), cex.main = 0.75)
		ty <- all.type == "D"
		points(all.ws[ty], all.Rrs[ty], pch = 1)
		ty <- all.type == "S"
		points(all.ws[ty], all.Rrs[ty], pch = 3, col = 2)
		legend("bottom", legend = c("Data", "Shifted"), pch = c(1, 3), col = c(1, 2))
		legend("topright", legend = paste(new.waves, paste("(<-", old.waves[nearest.cops.waves.index], ")", sep = "")), title = "Shifted", bty = "n", text.col = 2, cex = 0.7)

		plot(all.ws, all.Ed0, type = "l", xlab = "wavelength", ylab = expression(E[s]),
			main = c(dirname(specname), basename(specname)), cex.main = 0.75)
		ty <- all.type == "D"
		points(all.ws[ty], all.Ed0[ty], pch = 1)
		ty <- all.type == "S"
		points(all.ws[ty], all.Ed0[ty], pch = 3, col = 2)
		legend("bottom", legend = c("Data", "Shifted"), pch = c(1, 3), col = c(1, 2))
		legend("topright", legend = paste(new.waves, paste("(<-", old.waves[nearest.cops.waves.index], ")", sep = "")), title = "Shifted", bty = "n", text.col = 2, cex = 0.7)

		plot(all.ws, all.Lw, type = "l", xlab = "wavelength", ylab = expression(L[w]),
			main = c(dirname(specname), basename(specname)), cex.main = 0.75)
		ty <- all.type == "D"
		points(all.ws[ty], all.Lw[ty], pch = 1)
		ty <- all.type == "S"
		points(all.ws[ty], all.Lw[ty], pch = 3, col = 2)
		legend("bottom", legend = c("Data", "Shifted"), pch = c(1, 3), col = c(1, 2))
		legend("topright", legend = paste(new.waves, paste("(<-", old.waves[nearest.cops.waves.index], ")", sep = "")), title = "Shifted", bty = "n", text.col = 2, cex = 0.7)

		plot(all.ws, all.nLw, type = "l", xlab = "wavelength", ylab = expression(nL[w]),
			main = c(dirname(specname), basename(specname)), cex.main = 0.75)
		ty <- all.type == "D"
		points(all.ws[ty], all.nLw[ty], pch = 1)
		ty <- all.type == "S"
		points(all.ws[ty], all.nLw[ty], pch = 3, col = 2)
		legend("bottom", legend = c("Data", "Shifted"), pch = c(1, 3), col = c(1, 2))
		legend("topright", legend = paste(new.waves, paste("(<-", old.waves[nearest.cops.waves.index], ")", sep = "")), title = "Shifted", bty = "n", text.col = 2, cex = 0.7)

		write.table(file = outfile,
			data.frame(
				Ed0.waves = new.waves2, EdZ.waves = new.waves2, EuZ.waves = new.waves2,
				Ed0.th = Ed0.th.new.waves2,
				Ed0.0p = Ed0.new.waves2,
				Q.0 = Q.and.f$Q.0,
				Q.sun.nadir = Q.and.f$Q.sun.nadir,
				f.0 = Q.and.f$f.0,
				f.sun = Q.and.f$f.sun,
				Lw.0p = Lw.new.waves2,
				nLw.0p = nLw.new.waves2,
				Rrs.0p = Rrs.new.waves2
			),
			col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
	}
	dev.off()
}
