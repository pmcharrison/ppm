#' @useDynLib ppm, .registration=TRUE
NULL

#' @import Rcpp
NULL

#' @import methods
NULL

Rcpp::loadModule("unif_module", TRUE)
Rcpp::loadModule("ppm_simple", TRUE)
Rcpp::loadModule("ppm_decay", TRUE)
Rcpp::loadModule("record_decay", TRUE)

#' @name ppm_simple
#' @export
NULL

#' @name ppm_decay
#' @export
NULL
