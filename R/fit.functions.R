fit.with.loess <- function(waves, Depth, aop, span, depth.fitted,
                           idx.depth.0 = 1,
                           span.wave.correction = FALSE,
                           DEPTH.SPAN = FALSE,
                           minimum.obs = 3) {
	#browser()
  aop.fitted <- array(NA, dim = c(length(depth.fitted), ncol(aop)), dimnames = list(depth.fitted, colnames(aop)))
	aop.0 <- vector(mode = "numeric", length = ncol(aop))
	for (i in 1:length(waves)) {
		if(span.wave.correction) {
			span.w.corr <- min(650 / waves[i], 1)
      if (waves[i] < 420) {
        span.w.corr <- waves[i] /420
      }
		} else {
			 span.w.corr <- 1
		}

	  #### Check if there is any good data
	  # Added by Simon Belanger, 2016-02-11

		 if (all(is.infinite(aop[,i]) | is.na(aop[,i]))) {
      print(paste("No valid data at: ", waves[i]))

		 } else {
		   ix.good = which(!is.infinite(aop[,i]) & !is.na(aop[,i]))
		   if (length(ix.good)> minimum.obs ) { # ADDED

		     if (DEPTH.SPAN) {
		       actual.span=min(1,(span * span.w.corr)/diff(range(Depth[ix.good])))
		       fit.func <- loess(aop[ix.good,i] ~ Depth[ix.good], span = actual.span,
		                         control = loess.control(surface = "direct"))
		     } else {
		       fit.func <- loess(aop[ix.good,i] ~ Depth[ix.good], span = span * span.w.corr,
		                         control = loess.control(surface = "direct"))
		     }
		     #aop.fitted[,i] <- predict(fit.func, depth.fitted)
		     #aop.0[i] <- predict(fit.func, 0)
		     # fit only up to the depth at which depth.fitted is closest to first depth measured
		     aop.fitted[idx.depth.0:length(depth.fitted),i] <- predict(fit.func,
		                                                               depth.fitted[idx.depth.0:length(depth.fitted)])
		     aop.0[i] <- predict(fit.func, depth.fitted[idx.depth.0])

		     cat(waves[i], "(", span.w.corr, ")", " ")

		   } else {
		     print(paste("Not enough valid data for loess fitting at: ", waves[i]))
		   }
    }
	}
	cat("\n")
	aop.0[aop.0 == 0] <- NA
	list(aop.fitted = aop.fitted, aop.0 = aop.0)
}

