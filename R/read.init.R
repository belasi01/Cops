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
			if(init.param.name == "tiltmax.optics" |
			   init.param.name == "time.interval.for.smoothing.optics" |
			   init.param.name == "depth.interval.for.smoothing.optics" |
			   init.param.name == "sub.surface.removed.layer.optics" |
			   init.param.name == "delta.capteur.optics" |
			   init.param.name == "radius.instrument.optics" |
			   init.param.name == "linear.fit.Rsquared.threshold.optics" |
			   init.param.name == "linear.fit.max.delta.depth.optics") {
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

	#### Added to avoid failure when bandwidth is absent in the init file.
	if (is.null(cops.init$bandwidth)) {
	  print("WARNING: init file does not contain bandwidth parameter")
	  print("bandwidth is set to 10 nm")
	  bandwidth = 10
	  assign("bandwidth", 10, env = .GlobalEnv)
	  cops.init$bandwidth = bandwidth
	  # Update the init faile with new parameter
	  write.table(file = init.file, append = TRUE, cbind("bandwidth; numeric;", bandwidth), sep=" ", row.names = F, col.names = F, quote = F)
	}

	##### Added to avoid failure when linear fitting parameters are not provided.
	if (is.null(cops.init$linear.fit.Rsquared.threshold.optics)) {
	  print("WARNING: init file does not contain linear.fit.Rsquared.threshold.optics parameter")
	  print("linear.fit.Rsquared.threshold.optics is set to default values")
	  print("Add this line in the init file and EDIT: ")
	  print("linear.fit.Rsquared.threshold.optics; numeric; NA, 0.5, 0.6")
	  if (length(instruments.optics)==1) init.param.value <- 0.5
	  if (length(instruments.optics)==2) init.param.value <- c(NA,0.5)
	  if (length(instruments.optics)==3) init.param.value <- c(NA,0.5, 0.6)
	  if (length(instruments.optics)==4) init.param.value <- c(NA,0.5, 0.6, 0.6)
	  names(init.param.value) <- instruments.optics
	  assign("linear.fit.Rsquared.threshold.optics", init.param.value, env = .GlobalEnv)
	  cops.init$linear.fit.Rsquared.threshold.optics = linear.fit.Rsquared.threshold.optics
	  # Update the init faile with new parameter
	  write.table(file = init.file, append = TRUE, cbind("linear.fit.Rsquared.threshold.optics; numeric;", paste(as.character(linear.fit.Rsquared.threshold.optics), collapse=",")),
	              sep=" ", row.names = F, col.names = F, quote = F)
	}

	if (is.null(cops.init$linear.fit.max.delta.depth.optics)) {
	  print("WARNING: init file does not contain linear.fit.max.delta.depth.optics parameter")
	  print("linear.fit.max.delta.depth.optics is set to default values")
	  print("Add this line in the init file and EDIT: ")
	  print("linear.fit.max.delta.depth.optics; numeric; NA, 3, 2.5")
	  if (length(instruments.optics)==1) init.param.value <- 3
	  if (length(instruments.optics)==2) init.param.value <- c(NA,3)
	  if (length(instruments.optics)==3) init.param.value <- c(NA,3, 2.5)
	  if (length(instruments.optics)==4) init.param.value <- c(NA,3, 2.5, 2.5)
	  names(init.param.value) <- instruments.optics
	  assign("linear.fit.max.delta.depth.optics", init.param.value, env = .GlobalEnv)
	  cops.init$linear.fit.max.delta.depth.optics = linear.fit.max.delta.depth.optics
	  # Update the init faile with new parameter
	  write.table(file = init.file, append = TRUE, cbind("linear.fit.max.delta.depth.optics; numeric;", paste(as.character(linear.fit.max.delta.depth.optics), collapse=",")),
	              sep=" ", row.names = F, col.names = F, quote = F)
	}



	if (is.null(cops.init$windspeed_ms)) {
	  print("WARNING: init file does not contain windspeed_ms parameter")
	  print("windspeed_ms is set to default values")
	  print("Add this line in the init file and EDIT: ")
	  print("windspeed_ms; numeric; 4.0")
	  init.param.value <- 4
	  assign("windspeed_ms", init.param.value, env = .GlobalEnv)
	  cops.init$windspeed_ms = windspeed_ms
	  # Update the init faile with new parameter
	  write.table(file = init.file, append = TRUE, cbind("windspeed_ms; numeric;", windspeed_ms), sep=" ", row.names = F, col.names = F, quote = F)
	}




	cops.init
}
