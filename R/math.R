#' @name slope
#' @title slope
#' @export
slope = function(b2, a2, b1, a1) {
  return ((b2 - a2) / (b1 - a1))
}

#' @name cv
#' @title cv
#' @export
cv = function(x) {
  v = var(x)
  m = mean(x)
  return (v / m)
}


#' @name ratio
#' @title ratio
#' @export
ratio = function(vec, x) {
  count = sum(ifelse(vec == x, 1, 0), na.rm=T)
  total = length(vec)
  return(count / total)
}




#' @name mcnorm
#' @title mcnorm
#' @description
#' Generated Monte Carlo simulated data that follow normal distribution
#' @param n length, default = 100
#' @param s standard deviation, default = 1
#' @param m mean, default = 0
#' @param z offset, default = 0
#' @export
mcnorm = function(n=100, s=1, m=0, z=0) {
  h = (z + rnorm(n, 0, s))
  print.plot.norm(h)
  print.summary(h)
  return(h)
}
