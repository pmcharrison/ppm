#' @useDynLib ppm, .registration=TRUE
NULL

#' @import Rcpp
NULL

#' @import methods
NULL

Rcpp::loadModule("ppm", TRUE)

`.` <- NULL

#' @importFrom rlang ".data"
NULL
