#' Solar position as a function of date and time UTC and position (lat, lon)
#'
#'@param month is the month (1 to 12)
#'@param day is the day of the month (1-31)
#'@param tu is the time UTC in decimal format (0.0 to 23.999)
#'@param xlon is the longitude in decimal degrees
#'@param xlat is the latitude in decimal degrees
#'
#' @return Returns a vector os two numeric for the zenithal and azimuthal angles in degrees
#'     day is the number of the day in the month
#'
#'@author Bernard Gentilly
#'
possol <- function (month,day,tu,xlon,xlat) {

	ia = 0
	if (month <= 2) nojour=31*(month-1)+day else {
		if (month > 8) nojour=31*(month-1)-floor((month-2)/2)-2+day else
			nojour=31*(month-1)-floor((month-1)/2)-2+day
		if(ia != 0 && (ia %% 4) == 0) nojour=nojour+1
	}
	pi=3.14159265
	fac=pi/180.
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
