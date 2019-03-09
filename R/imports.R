#' @useDynLib ppm, .registration=TRUE
NULL

#' @import Rcpp
NULL

#' @import methods
NULL

Rcpp::loadModule("unif_module", TRUE)
Rcpp::loadModule("ppm", TRUE)

#' @name ppm_simple
#' @export
NULL

#' @name ppm_decay
#' @export
NULL
