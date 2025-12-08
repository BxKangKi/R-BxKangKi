#### Print Function Library

#' @param x Value
#' @title print.summary
#' @title print.summary
#' @description
#' Print summary and data length, mean, variance, standard deviation, and CV
#' @export
print.summary = function(x, matrix=FALSE, title = "") {
  cat(title)
  print(summary(x))
  v = var(x)
  m = mean(x)
  cat("length\n")
  print(length(x))
  cat("mean\n")
  print(m)
  cat("variance\n")
  print(v)
  cat("standard deviation\n")
  print(sqrt(v))
  cat("CV\n")
  print(cv(x))
  print(plot(x))
  if (matrix) {
    cat("correlation\n")
    print(cor(x))
    cat("covariance\n")
    print(cov(x))
  }
}
