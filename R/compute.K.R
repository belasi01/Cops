compute.K <- function(depth.fitted, idx.depth.0, aop.0, aop.fitted) {
  KZ.fitted <- -diff(log(aop.fitted)) / diff(depth.fitted)
  K0.fitted <- t(log(aop.0) - t(log(aop.fitted[-1, ]))) / (depth.fitted[-1]-depth.fitted[idx.depth.0])
	return(list(KZ.fitted = KZ.fitted, K0.fitted = K0.fitted))
}
