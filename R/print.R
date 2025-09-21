#### Print Function Library

#' @param title String
#' @param value Any Type
#' @name print.cat
#' @title print.cat
#' @description
#' Print title and value using cat and print function
#' @export
print.cat = function(title, value) {
  cat(title)
  print(value)
}

#' @param x Value
#' @title print.summary
#' @title print.summary
#' @description
#' Print summary and data length, mean, variance, standard deviation, and CV
#' @export
print.summary = function(x) {
  print(summary(x))
  v = var(x)
  m = mean(x)
  print.cat("Length\n", length(x))
  print.cat("Mean\n", m)
  print.cat("Variance\n", v)
  print.cat("Standard Deviation\n", sqrt(v))
  print.cat("CV\n", v / m)
}


#' @param vec Vector
#' @name print.corr
#' @title print.corr
#' @description
#' Print correlation of vector
#' @export
print.corr = function(vec) {
  print.cat("Correlation\n", cor(vec))
}


#' @param vec Vector
#' @name print.cov
#' @title print.cov
#' @description
#' Print covariance of vector
#' @export
print.cov = function(vec) {
  print.cat("Covariance\n", cov(vec))
}


#' @param x data.frame
#' @name print.vector.stat
#' @title print.vector.stat
#' @description
#' Print data frame statistic information
#' @export
print.data.frame.stat = function(data) {
  print.summary(data)
  print.corr(data)
  print.cov(data)
  print.plot(data)
}

#' @param a Any Type
#' @param b Any Type
#' @name print.stat2
#' @title print.stat2
#' @description
#' Print statistic information and relationship of two data
#' @export
print.stat2 = function(a, b) {
  vec = c(a,b)
  data = data.frame(
    x=a,
    y=b
  )
  print.summary(vec)
  print.corr(data)
  print.cov(data)
  print.plot.norm(vec)
}

#' @param names_vec String Vector
#' @param vec Vector
#' @name print.vector
#' @title print.vector
#' @description
#' Print vector with given names string vector
#' @export
print.vector = function(names_vec, vec) {
  names(vec) = names_vec
  print(vec)
}

#' @param x Any Value
#' @name print.plot
#' @title print.plot
#' @description
#' Print plots related normal distribution
#' @export
print.plot.norm = function(x) {
  par(mfrow = c(1, 2))
  qqnorm(x, main="Normal Q-Q Plot")
  hist(x, main="Histogram")
  print.summary(x)
}
