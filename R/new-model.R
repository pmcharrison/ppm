#' Create simple PPM model
#' 
#' Creates a simple PPM model, that is, a PPM model
#' without any non-traditional features such as memory decay.
#' 
#' @param alphabet_size
#' (Integerish scalar)
#' The size of the alphabet upon which the model will be trained and tested.
#' 
#' @param order_bound
#' (Integerish scalar)
#' The model's order bound. For example, an order bound of two means
#' that the model makes predictions based on the two preceding symbols.
#' 
#' @param shortest_deterministic
#' (Logical scalar)
#' If TRUE, the model will 'select' the shortest available order
#' that provides a deterministic prediction, if such an order exists,
#' otherwise defaulting to the longest available order.
#' For a given prediction, if this rule results in a lower model order
#' than would have otherwise been selected, 
#' then full counts (not update-excluded counts) will be used for 
#' the highest model order (but not for lower model orders).
#' This behaviour matches the implementations of PPM* in 
#' \insertCite{Pearce2005;textual}{ppm} and
#' \insertCite{Bunton1996;textual}{ppm}.
#' 
#' @param exclusion
#' (Logical scalar)
#' If TRUE, implements exclusion as defined in 
#' \insertCite{Pearce2005;textual}{ppm} and
#' \insertCite{Bunton1996;textual}{ppm}.
#' 
#' @param update_exclusion
#' (Logical scalar)
#' If TRUE, implements update exclusion as defined in 
#' \insertCite{Pearce2005;textual}{ppm} and
#' \insertCite{Bunton1996;textual}{ppm}.
#' 
#' @param escape
#' (Character scalar)
#' Takes values 'a', 'b', 'c', 'd', or 'ax',
#' corresponding to the eponymous escape methods 
#' in \insertCite{Pearce2005;textual}{ppm}.
#' 
#' @note
#' The implementation does not scale well to very large order bounds (> 50).
#' 
#' @references
#'   \insertAllCited{}
#'   
#' @return 
#' A PPM model object. 
#' These objects have reference semantics.
#' 
#' @seealso 
#' \code{\link{new_ppm_decay}},
#' \code{\link{model_seq}}.
#' 
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

#' Create decay-based PPM model
#' 
#' Creates a decay-based PPM model.
#' 
#' Decay-based PPM models generalise the PPM algorithm to incorporate
#' memory decay, where the effective counts of observed n-grams
#' decrease over time to reflect processes of auditory memory.
#' 
#' The model implements interpolated smoothing with escape method A, 
#' and explicitly disables exclusion and update exclusion.
#' 
#' @param alphabet_size
#' (Integerish scalar)
#' 
#' @param order_bound
#' (Integerish scalar)
#' 
#' @param buffer_length_time
#' (Numeric scalar)
#' 
#' @param buffer_length_items
#' (Integerish scalar)
#' 
#' @param buffer_weight
#' (Numeric scalar)
#' 
#' @param only_learn_from_buffer
#' (Logical scalar)
#' If TRUE, then n-grams are only learned if they fit within
#' the memory buffer.
#' 
#' @param stm_half_life
#' (Numeric scalar)
#' 
#' @param stm_weight
#' (Numeric scalar)
#' 
#' @param ltm_weight
#' (Numeric scalar)
#' 
#' @param noise
#' (Numeric scalar)
#' 
#' @return 
#' A PPM-decay model object. 
#' These objects have reference semantics.
#' 
#' @seealso 
#' \code{\link{new_ppm_simple}},
#' \code{\link{model_seq}}.
#' 
#' @export
new_ppm_decay <- function(
  alphabet_size,
  order_bound = 10L,
  buffer_length_time = 2.0,
  buffer_length_items = 13L,
  buffer_weight = 0.77,
  only_learn_from_buffer = TRUE,
  stm_half_life = 2.0,
  stm_weight = 0.53,
  ltm_weight = 0,
  noise = 0.5
) {
  checkmate::qassert(alphabet_size, "X1")
  checkmate::qassert(order_bound, "X[0,)")
  checkmate::qassert(buffer_length_time, "N1[0,)")
  checkmate::qassert(buffer_length_items, "X1[0,)")
  checkmate::qassert(buffer_weight, "N1[0,)")
  checkmate::qassert(only_learn_from_buffer, "B1")
  checkmate::qassert(stm_half_life, "N1(0,)")
  checkmate::qassert(stm_weight, "N1[0,)")
  checkmate::qassert(ltm_weight, "N1[0,)")
  checkmate::qassert(noise, "N1[0,)")
  
  decay_par = list(
    buffer_length_time = as.numeric(buffer_length_time),
    buffer_length_items = as.integer(buffer_length_items),
    buffer_weight = as.numeric(buffer_weight),
    only_learn_from_buffer = as.logical(only_learn_from_buffer),
    stm_half_life = as.numeric(stm_half_life),
    stm_weight = as.numeric(stm_weight),
    ltm_weight = as.numeric(ltm_weight),
    noise = as.numeric(noise)
  )
  
  new(
    ppm_decay, 
    alphabet_size = as.integer(alphabet_size),
    order_bound = as.integer(order_bound),
    decay_par = decay_par
  )
}

#' Is 'x' a 'ppm' object?
#'
#' Tests for objects of class "ppm".
#' 
#' @param x Object to test.
#' 
#' @return TRUE if the object is of class "ppm", FALSE otherwise.
#' @export
is_ppm <- function(x) {
  is_ppm_simple(x) || is_ppm_decay(x)
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
