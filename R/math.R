#' @name slope
#' @title slope
#' @export
slope = function(B, A, b, a) {
  return((B - A) / (b - a))
}

#' @name cv
#' @title cv
#' @export
cv = function(x) {
  v = var(x)
  m = mean(x)
  return (v / m)
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

#' @name corr
#' @title corr
#' @description
#' Generate correlated data with given parameters, either as a matrix or as a single vector.
#' @param n data length
#' @param corr target correlation
#' @param mu data mean vector (for matrix mode)
#' @param sd standard deviation (for vector mode)
#' @param m mean (for vector mode)
#' @param mode either "matrix" or "vector"
#' @export
corr <- function(n, corr, mu = c(0, 0), sd = 1, m = 0, mode = "matrix") {
  if (mode == "matrix") {
    R = matrix(c(1, corr, corr, 1), nrow = 2)
    U = chol(R)
    Gu = matrix(rnorm(n * 2), nrow = n, ncol = 2)
    Gc = Gu %*% U
    result = sweep(Gc, 2, mu, "+")
    print.corr(result)
    return(result)
  } else if (mode == "vector") {
    x = rnorm(n, m, sd)
    y = corr * scale(x) + sqrt(1 - corr^2) * rnorm(n, 0, 1)
    return(y)
  } else {
    stop("Mode must be 'matrix' or 'vector'")
  }
}
