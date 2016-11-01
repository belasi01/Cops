read.init <- function(init.file) {
	init.params <- scan(file = init.file, what = "", sep = "\n", quote = "", comment.char = "#")
	cops.init <- list()
	init.params.names <- NULL
	for(init.param in init.params) {
		dummy.param <- sub(" *$", "", sub("^ *", "", unlist(strsplit(init.param, ";"))))
		init.param.name <- dummy.param[1]
		init.param.type <- dummy.param[2]
		init.param.value <- unlist(strsplit(dummy.param[3], ","))
		if(init.param.type == "numeric") {
			if(init.param.name == "tiltmax.optics" | init.param.name == "time.interval.for.smoothing.optics" | init.param.name == "sub.surface.removed.layer.optics" | init.param.name == "delta.capteur.optics" | init.param.name == "radius.instrument.optics") {
				pv <- as.numeric(init.param.value)
				names(pv) <- instruments.optics
				assign(init.param.name, pv, env = .GlobalEnv)
			} else {
				assign(init.param.name, as.numeric(init.param.value), env = .GlobalEnv)
			}
		}
		if(init.param.type == "character") assign(init.param.name, as.character(init.param.value), env = .GlobalEnv)
		if(init.param.type == "logical") assign(init.param.name, as.logical(init.param.value), env = .GlobalEnv)
		cops.init <- c(cops.init, list(get(init.param.name)))
		init.params.names <- append(init.params.names, init.param.name)
	}
	names(cops.init) <- init.params.names
	cops.init
}
