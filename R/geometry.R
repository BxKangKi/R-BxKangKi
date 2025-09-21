#' @name geometry.slope
#' @title geometry.slope
#' @export
geometry.slope = function(B, A, b, a) {
  return((B - A) / (b - a))
}
