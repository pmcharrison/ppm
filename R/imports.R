#' @useDynLib ppm, .registration=TRUE
NULL

#' @import Rcpp
NULL

#' @import methods
NULL

Rcpp::loadModule("unif_module", TRUE)
Rcpp::loadModule("ppm_model", TRUE)

#' @name ppm_model
#' @export
NULL
