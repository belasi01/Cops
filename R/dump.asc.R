dump.asc <- function(dirasc, cops) {	
	instruments <- cops$instruments.optics
	for(instr in instruments) {
		f <- paste(dirasc, paste(instr, "txt", sep = "."), sep = "/")
		write.table(file = f, cbind(format(cops$dates), cops$Depth, as.numeric(cops$Depth.good), cops[[instr]]), col.names = c("date", "Depth", "valid", colnames(cops[[instr]])), row.names = FALSE, quote = FALSE, sep = "\t")
		instr.fitted <- paste(instr, "fitted", sep = ".")
		g <- paste(dirasc, paste(instr, "fitted", "txt", sep = "."), sep = "/")
		write.table(file = g, cbind(cops$depth.fitted, cops[[instr.fitted]]), col.names = c("depth", colnames(cops[[instr.fitted]])), row.names = FALSE, quote = FALSE, sep = "\t")
		KZ.instr.fitted <- paste("KZ", instr, "fitted", sep = ".")
		if(KZ.instr.fitted %in% names(cops)) {
			KZ.g <- paste(dirasc, paste("KZ", instr, "fitted", "txt", sep = "."), sep = "/")
			write.table(file = KZ.g, cbind(cops$depth.fitted[-1], cops[[KZ.instr.fitted]]), col.names = c("depth", colnames(cops[[KZ.instr.fitted]])), row.names = FALSE, quote = FALSE, sep = "\t")
		}
		K0.instr.fitted <- paste("K0", instr, "fitted", sep = ".")
		if(K0.instr.fitted %in% names(cops)) {
			K0.g <- paste(dirasc, paste("K0", instr, "fitted", "txt", sep = "."), sep = "/")
			write.table(file = K0.g, cbind(cops$depth.fitted[-1], cops[[K0.instr.fitted]]), col.names = c("depth", colnames(cops[[K0.instr.fitted]])), row.names = FALSE, quote = FALSE, sep = "\t")
		}
		instr.anc <- paste(instr, "anc", sep = ".")
		h <- paste(dirasc, paste(instr, "anc", "txt", sep = "."), sep = "/")
		write.table(file = h, cops[[instr.anc]], col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
	}
	instruments <- cops$instruments.others
	for(instr in instruments) {
		instr.anc <- paste(instr, "anc", sep = ".")
		h <- paste(dirasc, paste(instr, "anc", "txt", sep = "."), sep = "/")
		write.table(file = h, cops[[instr.anc]], col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
	}
	h <- paste(dirasc, paste("Others", "txt", sep = "."), sep = "/")
	write.table(file = h, cops[["Others"]], col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
	
	ns <- names(cops)
	i.nsv <- unlist(lapply(cops, is.vector))
	nsv <- ns[i.nsv]
	
	f <- paste(dirasc, "params.txt", sep = "/")
	unlink(f)
	for(n in nsv) {
		if(length(cops[[n]]) > 0.5 & length(cops[[n]]) < 2.5) {
			cat(file = f, append = TRUE, n, cops[[n]], sep = "\t")
			cat(file = f, append = TRUE, "\n")
		}
	}
	f <- paste(dirasc, "optics.txt", sep = "/")
	unlink(f)
	for(n in nsv) {
		if(length(cops[[n]]) == length(cops$instruments.optics)) {
			cat(file = f, append = TRUE, n, cops[[n]], sep = "\t")
			cat(file = f, append = TRUE, "\n")
		}
	}
	f <- paste(dirasc, "on.depth.txt", sep = "/")
	unlink(f)
	nsv2 <- NULL
	for(n in nsv) {
		if(length(cops[[n]]) == length(cops$Depth)) {
			nsv2 <- append(nsv2, n)
		}
	}
	write.table(file = f, as.data.frame.list(cops[nsv2]), col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
	f <- paste(dirasc, "on.waves.txt", sep = "/")
	unlink(f)
	nsv2 <- NULL
	for(n in nsv) {
		if(length(cops[[n]]) == length(cops$Ed0.waves)) {
			nsv2 <- append(nsv2, n)
		}
	}
	nsv2 <- nsv2[! nsv2 %in% "depth.discretization"]
	write.table(file = f, as.data.frame.list(cops[nsv2]), col.names = TRUE, row.names = FALSE, quote = FALSE, sep = "\t")
}
