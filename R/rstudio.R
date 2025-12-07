#' @name setcwd
#' @title setcwd
#' @description
#' Set 'current' working directory. "rstudioapi" required.
#' @export
setcwd = function() {
  library(rstudioapi)
  script_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(script_dir)
}
