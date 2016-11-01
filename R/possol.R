possol <- function (month,jday,tu,xlon,xlat) {
#     solar position (zenithal angle asol,azimuthal angle phi0
#                     in degrees)
#     jday is the number of the day in the month
	ia = 0
	if (month <= 2) nojour=31*(month-1)+jday else {
		if (month > 8) nojour=31*(month-1)-floor((month-2)/2)-2+jday else
			nojour=31*(month-1)-floor((month-1)/2)-2+jday
		if(ia != 0 && (ia %% 4) == 0) nojour=nojour+1
	}
	pi=3.14159265
	fac=pi/180.
#     solar position (zenithal angle asol,azimuthal angle phi0
#                     in degrees)
#     nojour is the day number in the year
#    mean solar time (heure decimale)
	tsm=tu+xlon/15.
	xla=xlat*fac
	tet=2.*pi*nojour/365.
#    time equation (in mn.dec)
	a1=.000075
	a2=.001868
	a3=.032077
	a4=.014615
	a5=.040849
	et=a1+a2*cos(tet)-a3*sin(tet)-a4*cos(2.*tet)-a5*sin(2.*tet)
	et=et*12.*60./pi
#     true solar time
	tsv=tsm+et/60.
	tsv=(tsv-12.)
#     hour angle
	ah=tsv*15.*fac
#     solar declination   (in radian)
	b1=.006918
	b2=.399912
	b3=.070257
	b4=.006758
	b5=.000907
	b6=.002697
	b7=.001480
	delta=b1-b2*cos(tet)+b3*sin(tet)-b4*cos(2.*tet)+b5*sin(2.*tet)-
		b6*cos(3.*tet)+b7*sin(3.*tet)
#     elevation,azimuth
	amuzero=sin(xla)*sin(delta)+cos(xla)*cos(delta)*cos(ah)
	elev=asin(amuzero)
	az=cos(delta)*sin(ah)/cos(elev)
	if ( (abs(az)-1.000) > 0.00000) az = sign(az)
	caz=(-cos(xla)*sin(delta)+sin(xla)*cos(delta)*cos(ah))/cos(elev)
	azim=asin(az)
	if(caz <= 0.) azim=pi-azim
	if(caz > 0 && az <= 0) azim=2*pi+azim
	azim=azim+pi
	pi2=2*pi
	if(azim > pi2) azim=azim-pi2
	elev=elev*180./pi
#     conversion in degrees
	asol=90.-elev
	phi0=azim/fac
	if(asol >  90) asol = -1
	return(c(asol,phi0))
}
