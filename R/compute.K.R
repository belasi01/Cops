compute.K <- function(depth.fitted, aop.0, aop.fitted) {
	KZ.fitted <- -diff(log(aop.fitted)) / diff(depth.fitted)
	K0.fitted <- t(log(aop.0) - t(log(aop.fitted[-1, ]))) / depth.fitted[-1]
	return(list(KZ.fitted = KZ.fitted, K0.fitted = K0.fitted))
}
