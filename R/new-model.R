#' @export
new_ppm_simple <- function(
  alphabet_size,
  order_bound = 10L,
  shortest_deterministic = TRUE,
  exclusion = TRUE,
  update_exclusion = TRUE,
  escape = "c"
) {
  checkmate::qassert(alphabet_size, "X1")
  checkmate::qassert(order_bound, "X[0,)")
  checkmate::qassert(shortest_deterministic, "B1")
  checkmate::qassert(exclusion, "B1")
  checkmate::qassert(update_exclusion, "B1")
  checkmate::qassert(escape, "S1")
  
  valid_escape_methods <- c("a", "b", "c", "d", "ax")
  if (!escape %in% valid_escape_methods)
    stop("escape parameter must be one of: ",
         paste(valid_escape_methods, collapse = ", "))
  
  new(
    ppm_simple, 
    alphabet_size = as.integer(alphabet_size),
    order_bound = as.integer(order_bound),
    shortest_deterministic = shortest_deterministic,
    exclusion = exclusion,
    update_exclusion = update_exclusion,
    escape = escape
  )
}

#' @export
new_ppm_decay <- function(
  alphabet_size,
  order_bound = 10L,
  buffer_length_items = 13L,
  buffer_weight = 0.77,
  stm_half_life = 2.0,
  stm_weight = 0.53,
  ltm_weight = 0,
  noise = 0.5
) {
  checkmate::qassert(alphabet_size, "X1")
  checkmate::qassert(order_bound, "X[0,)")
  checkmate::qassert(buffer_length_items, "X1[0,)")
  checkmate::qassert(buffer_weight, "N1[0,)")
  checkmate::qassert(stm_half_life, "N1(0,)")
  checkmate::qassert(stm_weight, "N1[0,)")
  checkmate::qassert(ltm_weight, "N1[0,)")
  checkmate::qassert(noise, "N1[0,)")
  
  decay_par = list(
    buffer_length_items = as.integer(buffer_length_items),
    buffer_weight = buffer_weight,
    stm_half_life = stm_half_life,
    stm_weight = stm_weight,
    ltm_weight = ltm_weight,
    noise = noise
  )
  
  new(
    ppm_decay, 
    alphabet_size = as.integer(alphabet_size),
    order_bound = as.integer(order_bound),
    decay_par = decay_par
  )
}

#' Is 'x' a 'ppm_simple' object?
#'
#' Tests for objects of class "ppm_simple".
#' 
#' @param x Object to test.
#' 
#' @return TRUE if the object is of class "ppm_simple", FALSE otherwise.
#' @export
is_ppm_simple <- function(x) {
  is(x, "Rcpp_ppm_simple")
}

#' Is 'x' a 'ppm_decay' object?
#'
#' Tests for objects of class "ppm_decay".
#' 
#' @param x Object to test.
#' 
#' @return TRUE if the object is of class "ppm_decay", FALSE otherwise.
#' @export
is_ppm_decay <- function(x) {
  is(x, "Rcpp_ppm_decay")
}
