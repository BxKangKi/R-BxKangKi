#' @name rstudio.setcwd
#' @title rstudio.setcwd
#' @export
rstudio.setcwd = function() {
  library(rstudioapi)
  script_dir = dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(script_dir)
}

