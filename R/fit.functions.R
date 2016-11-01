fit.with.loess <- function(waves, Depth, aop, span, depth.fitted, span.wave.correction = FALSE) {
	#browser()
  aop.fitted <- array(NA, dim = c(length(depth.fitted), ncol(aop)), dimnames = list(depth.fitted, colnames(aop)))
	aop.0 <- vector(mode = "numeric", length = ncol(aop))
	for (i in 1:length(waves)) {
    print(i)
		if(span.wave.correction) {
			span.w.corr <- min(650 / waves[i], 1)
      if (waves[i] < 420) {
        span.w.corr <- waves[i] /420
      }
		} else {
			 span.w.corr <- 1
		}
	  
	  #### Check if there is any good data
	  # Added by Simon Bélanger, 2016-02-11
	  
		 if (all(is.infinite(aop[,i]) | is.na(aop[,i]))) {
      print(paste("No valid data at: ", waves[i]))
      
		 } else {
		   ix.good = which(!is.infinite(aop[,i]) & !is.na(aop[,i])) 
		   if (length(ix.good)> 3 ) { # ADDED
		     
		     fit.func <- loess(aop[ix.good,i] ~ Depth[ix.good], span = span * span.w.corr, 
		                       control = loess.control(surface = "direct")) 
		     aop.fitted[,i] <- predict(fit.func, depth.fitted)
		     aop.0[i] <- predict(fit.func, 0)
		     cat(waves[i], "(", span.w.corr, ")", " ")
		     
		   } else {
		     print(paste("Not enough valid data at: ", waves[i]))
		   }
    }
	}
	cat("\n")
	list(aop.fitted = aop.fitted, aop.0 = aop.0)
}
