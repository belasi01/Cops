#
# ******************************************************************
GreggCarder.f <- function(jday, rlon, rlat, lam.sel = 350:700,
	the = -99, hr = -99, Vi = 15,am = 1,
	wsm = 4, ws = 6, pres = 1013.25, rh = 80, wv = 1.5,
	debug = F) {
#
#  Computes spectral solar irradiance just below the surface given
#  meteorological data (interactively) and writes to data file.
#  Direct and diffuse components are broken out separately.
#  Watson W. Gregg, NASA/Goddard Space Flight Center (301) 286-3464.
#
#Julian day
#longitude (W negative)
#latitude (N positive)
#solar zenith angle -99 to calculate from day and earth position
#time in hours GMT
#visibility in km 15 for default
#air mass (1-10) 1 for default
#mean wind speed, current wind speed (m/s) 4.0, 6.0 for default
#pressure in millibars 1013.25 for standard
#relative humidity 80 for default
#Enter water vapor in cm 1.5 for default
#
      pi2 = 2.0*pi
      rad = 180.0/pi
      h = 6.6256E-34
      c = 2.998E8
      hc = 1.0/(h*c)
#  Compute ozone scale height in cm
      to3 = 235.0 + (150.0+40.0*sin(0.9865*(jday-30.0)) +
     	     20.0*sin(3.0*(rlon)))*sin(1.28*rlat)*sin(1.28*rlat)
      sco3 = to3*1.0E-3
      if (the < -90) {
       if(hr < -90) {
	cat("you must specify either solar zenith angle either time in hours GMT\n")
	return(NA)
       }
       sunz = GreggCarder.sunang(jday,hr,rad,rlon,rlat)
       theta = sunz
      } else {
       theta = the
      }
      if(debug) {
      cat("Ozone climatological values will be computed from the day and Earth position\n")
      cat(c("Theta = ",theta,"      Air mass   = ",am,
      "Ozone = ",to3," DBU","\n  Visibility = ",Vi," km",
      "RH = ",rh," %","    Pressure   = ",pres," mb",
      "\nwsbar = ",wsm,"      ws = ",ws," m/s",
      "Water Vapor = ",wv," cm\n"))
      }
      if (theta > 90.0){
       cat("Sun is below horizon -- irradiance = 0\n")
       return(NA)
      }
#
#  Read in extraterrestrial light
      Fobar = approx(GreggCarder.d$lam,GreggCarder.d$Fobar,lam.sel)$y
      oza = approx(GreggCarder.d$lam,GreggCarder.d$oza,lam.sel)$y
      ag = approx(GreggCarder.d$lam,GreggCarder.d$ag,lam.sel)$y
      aw = approx(GreggCarder.d$lam,GreggCarder.d$aw,lam.sel)$y
      lam = lam.sel
#
#  Correct for Earth-Sun distance
       Fo = Fobar * (1.0+1.67E-2*cos(pi2*(jday-3)/365.0))^2
#
#  Compute irradiance
      gc.atmodd <- GreggCarder.atmodd(rad,lam,theta,oza,ag,aw,sco3,pres,wv,
	rh,am,wsm,ws,Vi,Fo)
      sumirr = sum(gc.atmodd$Ed)
      if(debug) {
      cat(c("Total irradiance = ",sumirr," (W/m2)\n"))
      }
#  PAR
      sumpar = 1.0E-9 * hc/6.023E17 * sum(gc.atmodd$Ed * lam)
      if(debug) {
      cat(c("PAR (350-700)    = ",sumpar," (uE/m2/s)\n"))
      }
#
      return(list(lam = lam, Edir = gc.atmodd$Edir, Edif = gc.atmodd$Edif, Ed = gc.atmodd$Ed))
}
GreggCarder.atmodd <- function(rad,lam,theta,oza,ag,aw,sco3,p,wv,
	rh,am,wsm,ws,vis,Fo) {
#
#  Model for atmospheric transmittance of solar irradiance through
#  a maritime atmosphere.  Computes direct and diffuse separately.
#  Includes water vapor and oxygen absorption.
#
      p0 = 1013.25
#
#  Compute atmospheric path lengths (air mass); pressure-corrected
      cosunz = cos(theta/rad)
      rex = -1.253
      rtmp = (93.885-theta)^rex
      rm = 1.0/(cosunz+0.15*rtmp)
      rmp = p/p0*rm
      otmp = (cosunz*cosunz+44.0/6370.0)^0.5
      rmo = (1.0+22.0/6370.0)/otmp
#
#  Obtain aerosol parameters; simplified Navy aerosol model
      gc.navaer <- GreggCarder.navaer(rh,am,wsm,ws,vis)
      eta = -gc.navaer$alpha
#   Forward scattering probability
      alg = log(1.0-gc.navaer$asymp)
      afs = alg*(1.459+alg*(.1595+alg*.4129))
      bfs = alg*(.0783+alg*(-.3824-alg*.5874))
      Fa = 1.0 - 0.5*exp((afs+bfs*cosunz)*cosunz)
#
#  Surface reflectance
      gc.sfcrfl <- GreggCarder.sfcrfl(rad,theta,ws)
#
#  Compute spectral irradiance
#    Rayleigh, by Bird's method
       rlam = lam*1.0E-3
       tr = 1.0/(115.6406*rlam^4 - 1.335*rlam^2)
       rtra = exp(-tr*rmp)   #transmittance
#    Ozone
       to = oza*sco3   #optical thickness
       otra = exp(-to*rmo)   #transmittance
#   Aerosols
       ta = gc.navaer$beta*rlam^eta
       atra = exp(-ta*rm)
       taa = exp(-(1.0-gc.navaer$wa)*ta*rm)
       tas = exp(-gc.navaer$wa*ta*rm)
#   Oxygen/gases
       gtmp = (1.0 + 118.3*ag*rmp)^0.45
       gtmp2 = -1.41*ag*rmp
       gtra = exp(gtmp2/gtmp)
#   Water Vapor
       wtmp = (1.0+20.07*aw*wv*rm)^0.45
       wtmp2 = -0.2385*aw*wv*rm
       wtra = exp(wtmp2/wtmp)
#
#  Direct irradiance
       Edir = Fo*cosunz*rtra*otra*atra*gtra*wtra
#
#   Diffuse irradiance
       dray = Fo*cosunz*gtra*wtra*otra*taa*0.5*
     	      (1.0-rtra^.95)
       daer = Fo*cosunz*gtra*wtra*otra*rtra^1.5*taa*Fa*
     	      (1.0-tas)
#
#  Total diffuse
       Edif = dray + daer
#
       Ed = Edir + Edif
#
      return(list(Edir = Edir, Edif = Edif, Ed = Ed))
}
#
# *****************************************************************
GreggCarder.navaer <- function(rh,am,wsm,ws,vis) {
#
#  Computes aerosol parameters according to a simplified version
#  of the Navy marine aerosol model.
#
      a <- 0
      dndr <- 0
      ro = c(0.03,0.24,2.0)
      r = c(0.1,1.0,10.0)
#
      rlam = 0.55
#
#  Relative humidity factor
      if (rh >= 100.0)rh = 99.9
      rnum = 2.0 - rh/100.0
      rden = 6.0*(1.0-rh/100.0)
      frh = (rnum/rden)^0.333
#
#  Size distribution amplitude components
      a[1] = 2000.0*am*am
      a[2] = 5.866*(wsm-2.2)
      if (a[2] < 0.5)a[2] = 0.5
      a[3] = 0.01527*(ws-2.2)*0.05	  #from Hughes 1987
      if (a[3] < 1.4E-5)a[3] = 1.4E-5
#
#  Compute size distribution at three selected radii according to
#  Navy method
      for(n in 1:3) {
       dndr[n] = 0.0
       for(i in 1:3) {
	rden = frh*ro[i]
	arg = log(r[n]/rden)*log(r[n]/rden)
	rval = a[i]*exp(-arg)/frh
	dndr[n] = dndr[n] + rval
       }
      }
#
#  Least squares approximation
      sumx = 0.0
      sumy = 0.0
      sumxy = 0.0
      sumx2 = 0.0
      for(n in 1:3) {
       sumx = sumx + log10(r[n])
       sumy = sumy + log10(dndr[n])
       sumxy = sumxy + log10(r[n])*log10(dndr[n])
       sumx2 = sumx2 + log10(r[n])*log10(r[n])
      }
      gama = sumxy/sumx2
      rlogc = sumy/3.0 - gama*sumx/3.0
      alpha = -(gama+3.0)
#
#  Compute beta
      cext = 3.91/vis
      beta = cext*rlam^alpha
#
#  Compute asymmetry parameter -- a function of alpha
      if (alpha > 1.2) {
       asymp = 0.65
      } else {
       if (alpha < 0.0) {
        asymp = 0.82
       } else {
        asymp = -0.14167*alpha + 0.82
       }
      }
#
#  Single scattering albedo at 550; function of RH
      w0 = (-0.0032*am + 0.972)*exp(3.06E-4*rh)
#
      return(list(beta = beta, alpha = alpha, wa = w0, asymp = asymp))
}
#
# ***************************************************************
GreggCarder.sfcrfl <- function(rad,theta,ws) {
#
#  Computes surface reflectance for direct (rod) and diffuse (ros)
#  components separately, as a function of theta, wind speed or
#  stress.
#
      rn = 1.341    #index of refraction of pure seawater
      roair = 1.2E3	 #density of air g/m3
#
#  Foam and diffuse reflectance
      if (ws > 4.0){
       if (ws <= 7.0){
	cn = 6.2E-4 + 1.56E-3/ws
	rof = roair*cn*2.2E-5*ws*ws - 4.0E-4
       }else{
	cn = 0.49E-3 + 0.065E-3*ws
	rof = (roair*cn*4.5E-5 - 4.0E-5)*ws*ws
       }
       rosps = 0.057
      } else {
       rof = 0.0
       rosps = 0.066
      }
#
#  Direct
#   Fresnel reflectance for theta < 50, ws < 2 m/s
      if (theta < 50.0 | ws < 2.0) {
       if (theta == 0.0) {
	rospd = 0.0211
       } else {
	rtheta = theta/rad
	sintr = sin(rtheta)/rn
	rthetar = asin(sintr)
	rmin = rtheta - rthetar
	rpls = rtheta + rthetar
	sinp = (sin(rmin)*sin(rmin))/(sin(rpls)*sin(rpls))
	tanp = (tan(rmin)*tan(rmin))/(tan(rpls)*tan(rpls))
	rospd = 0.5*(sinp + tanp)
       }
      } else {
#   Empirical fit otherwise
       a = 5.25E-4*ws + 0.065
       b = -1.67E-3*ws + 0.074
       rospd = a*exp(b*(theta-60.0))
      }
#
#  Reflectance totals
      rod = rospd + rof
      ros = rosps + rof
#
      return(list(rod = rod, ros = ros))
}
#
# ***************************************************************
GreggCarder.sunang <- function(iday,hr,rad,xlon,ylat) {
#
#  Computes sun azimuth and zenith angles for a given
#  time, date, latitude and longitude.  This program
#  is from the NMFS ELAS computer code.  Modified for
#  standard coordinates (W long. negative), to correct
#  for dateline problem, and to correct coefficients (taken
#  from Iqbal, 1983, An Introduction to Solar Radiation).
#  Watson Gregg, Research and Data Systems, Corp.
#
#   iday = julian day
#   time = time of day in seconds
#   ylat = latitude of pixel
#   xlon = longitude of pixel
#   sunz = solar zenith in degrees
#   sdec = solar declination angle in degrees
#   thez = theta zero orbital position in degrees
#   tc = time correction
#   xha = solar hour angle in degrees
#
#  Compute solar declination angle
      thez = 360.0*(iday-1)/365.0
      rthez = thez/rad
      sdec = 0.396372-22.91327*cos(rthez) + 4.02543*sin(rthez)
        - 0.387205*cos(2.0*rthez) + 0.051967*sin(2.0*rthez)
     	 - 0.154527*cos(3.0*rthez) + 0.084798*sin(3.0*rthez)
      rsdec = sdec/rad
#
#  Time correction for solar hour angle, and solar hour angle
      tc = 0.004297 + 0.107029*cos(rthez) - 1.837877*sin(rthez)
     	 - 0.837378*cos(2.0*rthez) - 2.342824*sin(2.0*rthez)
      xha = (hr-12.0)*15.0 + xlon + tc
      if (xha >  180.0)xha = xha-360.0
      if (xha < -180.0)xha = xha+360.0
      rlat = ylat/rad
      rlon = xlon/rad
      rha = xha/rad
#
#  Sun zenith
      costmp = sin(rlat)*sin(rsdec) +
     	       cos(rlat)*cos(rsdec)*cos(rha)
      rsunz = acos(costmp)
#
#  Convert to degrees
      sunz = rsunz*rad
#
      return(sunz)
}
