get_weight <- function(model, n_gram, pos, time, update_excluded = FALSE) {
  stopifnot(is_ppm(model))
  checkmate::qassert(n_gram, "X[0,)")
  checkmate::qassert(pos, "X1[0,)")
  checkmate::qassert(time, "N1()")
  checkmate::qassert(update_excluded, "B1")
  if (is_ppm_decay(model) && update_excluded) 
    stop("update-excluded counts are not stored for decay-based models")
  
  model$get_weight(
    n_gram = as.integer(n_gram),
    pos = pos,
    time = time,
    update_excluded = update_excluded
  )
}
