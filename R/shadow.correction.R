shadow.correction <- function(instr, cops, SB=NA) {
	mymessage("shadow correction")
	chl <- cops$chl
	SHADOW.CORRECTION <- cops$SHADOW.CORRECTION
	SHADOW.CORRECTION.FROM.KD <- NA
	absorption.waves <- cops$absorption.waves
	absorption.values <- cops$absorption.values
	date.mean <- cops$date.mean
	sunzen <- cops$sunzen
	longitude <- cops$longitude
	latitude <- cops$latitude
	waves <- cops[[paste(instr, "waves", sep = ".")]]
	julian.day <- as.numeric(format(date.mean, format = "%j"))
	if(!is.na(chl) && chl != 999) { #### Use Morel and Maritorena model to
	                   #### get absorption from CHL, but if 999 then use the Kd derived absorption
# 350-700
		chlTMP <- chl
		chlTMP <- min(chlTMP, 9.99999)
		chlTMP <- max(chlTMP, 0.03001)
		wavesTMP <- waves
		wavesTMP[wavesTMP > 349.999 & wavesTMP < 350.001] <- 350.001
		wavesTMP[wavesTMP > 699.999 & wavesTMP < 700.001] <- 699.999
		i.wavesSUP <- wavesTMP < 350 | wavesTMP > 700
		wavesTMP[i.wavesSUP] <- 550
		aR <- as.vector(popt.fv.a(wavesTMP, chlTMP)) * radius.instrument.optics[instr]
		aR[i.wavesSUP] <- NA
# > 700
		wavesTMP <- waves
		wavesTMP[wavesTMP > 699.999 & wavesTMP < 700.001] <- 699.999
		wavesTMP[wavesTMP > 864.999] <- 864.999
		i.wavesIR <-  wavesTMP > 700
		aR[i.wavesIR] <- popt.f.aw(wavesTMP[i.wavesIR]) * radius.instrument.optics[instr]
# < 350
		wavesTMP <- waves
		wavesTMP[wavesTMP > 349.999 & wavesTMP < 350.001] <- 350.001
		i.wavesUV <- wavesTMP < 350
		aR[i.wavesUV] <- popt.f.a(350.001, chlTMP) * radius.instrument.optics[instr]
	} else {
    if (!is.na(chl) && chl == 999) { #### CHL == 999 mean used Kd-derived absorption

      ##### This correction needs an estimation of Kd and R.
      ##### It uses linear fit if valid, and loess estimation
      ##### if any linear fit failed (for either Lu and Ed!)

      ##### Begin with LuZ
      if (!is.null(cops$LuZ.0m.linear)) {
        Q  = 4 # this is an approximation of the Q-Factor for the
        # calulation of the sub-surface irradiance reflectance
        #### Check for a valid Kd with linear interpolation
        ix.LuZ.0m.valid = which(!is.na(cops$LuZ.0m))


        if (anyNA(cops$K.EdZ.surf[ix.LuZ.0m.valid]) |
            anyNA(cops$K.LuZ.surf[ix.LuZ.0m.valid])) {
        ####
        #### I change the condition because I don't understand why we were checking NA data in the cops$K.LuZ.surf[ix.LuZ.0m.valid]
        #### Update: Ok I understand why.... it's because R0- needs a valid value of LuZ.0m valid from the linear interpolation
          #####. going back to the original code
        #if (anyNA(cops$K.EdZ.surf[ix.LuZ.0m.valid])) { #### We use LOESS only if the linear Kd fails
        #####
          # check for the depth used for the LuZ extrapolation
          #max.depth <- max(cops$LuZ.Z.interval, na.rm = T)
          #ix.max.depth <- which.min(abs(cops$depth.fitted - max.depth))
          ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
          if (ix.2.5 > length(cops$K0.EdZ.fitted[,1])) ix.2.5 <- length(cops$K0.EdZ.fitted[,1]) # added for shallow profiles
          Kd = cops$K0.EdZ.fitted[ix.2.5,]
          x = 0
          while (anyNA(Kd[ix.LuZ.0m.valid])) { # This condition was added because some time the EdZ.fitted extrapolation fail at the end of a profile
            x = 1 + x
            Kd = cops$K0.EdZ.fitted[ix.2.5-x,]
          }

          Ed.0m = cops$EdZ.0m
          R = (cops$LuZ.0m*Q)/Ed.0m
          SHADOW.CORRECTION.FROM.KD <- "LOESS"
        } else {
          Kd = cops$K.EdZ.surf
          Ed.0m = cops$EdZ.0m.linear
          R = (cops$LuZ.0m.linear*Q)/Ed.0m
          SHADOW.CORRECTION.FROM.KD <- "LINEAR"
        }
        # I think this condition is not necessary any more...
        #if (length(ix.LuZ.0m.valid) == 0) {
          ### This is when SHADOW.CORRECTION == 999
          ### but the linear fit failed at all bands....
          ### The Kd for the to 2.5 meter is assumed.
        #  ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
        #  Kd = cops$K0.EdZ.fitted[ix.2.5,]
        #  Ed.0m = cops$EdZ.0m
        #  R = (cops$LuZ.0m*Q)/Ed.0m
        #  SHADOW.CORRECTION.FROM.KD <- "LOESS"
        #}

      }

      ##### With EuZ
      if (!is.null(cops$EuZ.0m.linear)) {
        #### Check for a valid Kd with linear interpolation
        ix.EuZ.0m.valid = which(!is.na(cops$EuZ.0m))
       if (anyNA(cops$K.EdZ.surf[ix.EuZ.0m.valid]) |
            anyNA(cops$K.EuZ.surf[ix.EuZ.0m.valid])) {

          # check for the depth used for the LuZ extrapolation
          ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
          if (ix.2.5 > length(cops$K0.EdZ.fitted[,1])) ix.2.5 <- length(cops$K0.EdZ.fitted[,1])  # added for shallow profiles
          Kd = cops$K0.EdZ.fitted[ix.2.5,]
          x = 0
          while (anyNA(Kd[ix.EuZ.0m.valid])) { # This condition was added because some time the EdZ.fitted extrapolation fail at the end of a profile
            x = 1 + x
            Kd = cops$K0.EdZ.fitted[ix.2.5-x,]
          }
          Ed.0m = cops$EdZ.0m
          R = (cops$EuZ.0m)/Ed.0m
          SHADOW.CORRECTION.FROM.KD <- "LOESS"
        } else {
          Kd = cops$K.EdZ.surf
          Ed.0m = cops$EdZ.0m.linear
          R = cops$EuZ.0m.linear/Ed.0m
          SHADOW.CORRECTION.FROM.KD <- "LINEAR"
        }
        if (length(ix.EuZ.0m.valid) == 0) {
          ### This is when SHADOW.CORRECTION == 999
          ### but the linear fit failed at all bands....
          ### The Kd for the to 2.5 meter is assumed.
          ix.2.5 <- which.min(abs(cops$depth.fitted - 2.5))
          if (ix.2.5 > length(cops$K0.EdZ.fitted[,1])) ix.2.5 <- length(cops$K0.EdZ.fitted[,1])
          Kd = cops$K0.EdZ.fitted[ix.2.5,]
          Ed.0m = cops$EdZ.0m
          R = (cops$EuZ.0m)/Ed.0m
          SHADOW.CORRECTION.FROM.KD <- "LOESS"
        }
      }

	    # From Morel and Maritorena JGR 2001 eq 8'
	    absorption.values <- Kd*0.9*(1-R)/(1+2.25*R)
	    cops$absorption.values <- absorption.values
	    aR <- absorption.values * radius.instrument.optics[instr]
	  } else {
	    agree.waves <- isTRUE(all.equal(absorption.waves, waves, scale = 1, tolerance = 1))
	    if(!agree.waves) {
	      cat("wavelengths in file absorption.cops.dat do not agree with wavelengths of", instr, "instrument\n")
	      cat(instr, ":", waves, "\n")
	      cat("file absorption.cops.dat", ":", absorption.waves, "\n")
	      stop()
	    }
	    aR <- absorption.values * radius.instrument.optics[instr]
	  }
	}
	names(aR) <- waves

	### Add the case when the Shadow Band data is available (Simon Belanger 2018/12/15)
	if (is.na(SB)[1]) {
	  # This code was added to improve the Edif and Edir estimation using Gregg and Carder model
	  # The visibility is reduced to match the measured Ed0.0p obtained from the COPS reference sensor
	  print ("No Bioshade measurements")
	  print ("Compute Ed0.dif/Ed0.tot using Gregg and Carder model")
	  julian.day <- as.numeric(format(cops$date.mean, format = "%j"))
	  visibility <- 25
	  egc <- GreggCarder.f(julian.day, longitude, latitude, sunzen, lam.sel = waves, Vi=visibility)

	  ix.490 <- which.min(abs(waves - 490))

	  ratio = egc$Ed[ix.490]*100/cops$Ed0.0p[ix.490]

	  while (ratio > 1.05  & visibility > 0.5) {
	    egc <- GreggCarder.f(julian.day, longitude, latitude, sunzen,lam.sel = waves, Vi=visibility)
	    ratio = egc$Ed[ix.490]*100/cops$Ed0.0p[ix.490]
	    visibility = visibility - 0.5
	  }

	  Edif <- egc$Edif*100 # factor 100 to convert into same COPS units
	  Edir <- egc$Edir*100
	} else {
	  Edif <- SB$Ed0.dif
	  Edir <- SB$Ed0.tot - Edif
	}
	###
	ratio.edsky.edsun <- Edif / Edir
	epss <- shadow.epsilon(instr, aR, sunzen, ratio.edsky.edsun)
	eps.sun <- epss$eps.sun
	eps.sky <- epss$eps.sky
	eps <- epss$eps
	correction <- 1 - eps
	corr.names <- c("shad.aR", "shad.Edif", "shad.Edir", "shad.ratio.edsky.edsun",
	                "shad.eps.sun", "shad.eps.sky", "shad.eps", "shad.correction")
	corr.names <- paste(instr, corr.names, sep = ".")
	corr.names <- c(corr.names,"absorption.values", "absorption.waves", "SHADOW.CORRECTION.FROM.KD")
	corr.list <- list(aR, Edif, Edir, ratio.edsky.edsun, eps.sun,
	                  eps.sky, eps, correction, absorption.values, waves, SHADOW.CORRECTION.FROM.KD)
	names(corr.list) <- corr.names
	corr.list
}
