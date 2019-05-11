#' Get weight
#' 
#' Gets the weight (or count) of a given n-gram in a trained PPM moel.
#' 
#' @param model
#' A PPM model object as produced by (for example)
#' \code{\link{new_ppm_simple}} or \code{\link{new_ppm_decay}}.
#' 
#' @param n_gram 
#' An integerish vector defining the n-gram to be queried.
#' 
#' @param pos
#' (Integerish scalar)
#' The nominal 'position' at which the n-gram is retrieved
#' (only relevant for decay-based models).
#' 
#' @param time
#' (Numeric scalar)
#' The nominal 'time' at which the n-gram is retrieved
#' (only relevant for decay-based models).
#' 
#' @param update_excluded
#' (Logical scalar)
#' Whether to retrieve update-excluded counts or not.
#' 
#' @param zero_indexed
#' (Logical scalar)
#' Whether the input n-gram is zero-indexed.
#' 
#' @return A numeric scalar identifying the weight of the n-gram.
#' 
#' @export
get_weight <- function(model, n_gram, pos = 1L, time = 0, update_excluded = FALSE,
                       zero_indexed = FALSE) {
  stopifnot(is_ppm(model))
  checkmate::qassert(n_gram, "X[0,)")
  checkmate::qassert(pos, "X1[0,)")
  checkmate::qassert(time, "N1()")
  checkmate::qassert(update_excluded, "B1")
  if (is_ppm_decay(model) && update_excluded) 
    stop("update-excluded counts are not stored for decay-based models")
  checkmate::qassert(zero_indexed, "B1")
  
  key <- as.integer(if (zero_indexed) n_gram else n_gram - 1L)
  
  model$get_weight(
    n_gram = key,
    pos = pos,
    time = time,
    update_excluded = update_excluded
  )
}
