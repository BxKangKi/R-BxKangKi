#' @name mc.corr
#' @title mc.corr
#' @description
#' Generate 2 rows matrix that correlation is almost fit to target value
#' @param n data length
#' @param corr target correlation
#' @param mu data mean vector
#' @export
mc.corr = function(n, corr, mu = c(0, 0)) {
  R = matrix(c(1, corr, corr, 1), nrow = 2)
  U = chol(R)
  Gu = matrix(rnorm(n * 2), nrow = n, ncol = 2)
  Gc = Gu %*% U
  result = sweep(Gc, 2, mu, "+")
  print.corr(result)
  return(result)
}

#' @name mc.norm
#' @title mc.norm
#' @description
#' Generated Monte Carlo simulated data that follow normal distribution
#' @param n length, default = 100
#' @param s standard deviation, default = 1
#' @param m mean, default = 0
#' @param z offset, default = 0
#' @export
mc.norm = function(n=100, s=1, m=0, z=0) {
  h = (z + rnorm(n, 0, s))
  print.plot.norm(h)
  print.summary(h)
  return(h)
}

#' @name mc.corr.norm
#' @title mc.corr.norm
#' @description
#' Generated Monte Carlo simulated data with given normal distribution information and target correlation
#' @param r target correlation
#' @param n length, default = 100
#' @param sd standard deviation, default = 1
#' @param m mean, default = 0
#' @export
mc.corr.norm = function(r, n, m=0, sd=1) {
  x = rnorm(n, m, sd)
  y = r * scale(x) + sqrt(1 - r^2) * scale(x)
  return(y)
}
