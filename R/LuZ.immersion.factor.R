LuZ.immersion.factor <- function(waves) {
  data(LuZ.immersion.factor)
  return(spline(df$waves, df$immersion, xout=waves, method = "natural")$y)
}
