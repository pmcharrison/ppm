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
#' The model's Markov order bound. For example, an order bound of two means
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
#' The weight of a given n-gram over time is determined by a \emph{decay kernel}.
#' This decay kernel is parametrised by the arguments
#' \eqn{w_0}, \eqn{w_1}, \eqn{w_2}, \eqn{w_\infty},
#' \eqn{n_b}, \eqn{t_b}, \eqn{t_1}, \eqn{t_2}, \eqn{\sigma_\epsilon}
#' (see above).
#' These parameters combine to define a decay kernel of the following form:
#' 
#' \figure{example-decay-kernel.png}{options: width=450}
#' 
#' The decay kernel has three phases:
#' 
#' - Buffer (yellow);
#' - Short-term memory (red);
#' - Long-term mermory (blue).
#' 
#' While within the buffer, the n-gram has weight \eqn{w_0}.
#' The buffer has limited temporal and itemwise capacity.
#' In particular, an n-gram will leave the buffer once one 
#' of two conditions is satisfied:
#' 
#' - A set amount of time, \eqn{t_b}, elapses since the first symbol in the n-gram was observed, or
#' - The buffer exceeds the number of symbols it can store, \eqn{n_b},
#' and the n-gram no longer fits completely in the buffer,
#' having been displaced by new symbols.
#' 
#' There are some subtleties about how this actually works in practice,
#' refer to \insertCite{Harrison2019;textual}{ppm} for details.
#' 
#' The second phase, short-term memory, begins as soon as the 
#' buffer phase completes. It has a fixed temporal duration
#' of \eqn{t_1}. At the beginning of this phase,
#' the n-gram has weight \eqn{w_1};
#' during this phase, its weight decays exponentially until it reaches
#' \eqn{w_2} at timepoint \eqn{t_2}.
#' 
#' The second phase, long-term memory, begins as soon as the 
#' short-term memory phase completes. It has an unlimited temporal duration.
#' At the beginning of this phase,
#' the n-gram has weight \eqn{w_2};
#' during this phase, its weight decays exponentially
#' to an asymptote of \eqn{w_\infty}.
#' 
#' The model optionally implements Gaussian noise at the weight retrieval stage.
#' This Gaussian is parametrised by the standard deviation parameter
#' \eqn{\sigma_\epsilon}.
#' See \insertCite{Harrison2019;textual}{ppm} for details. 
#' 
#' This function supports simpler decay functions with fewer stages;
#' in fact, the default parameters define a one-stage decay function,
#' corresponding to a simple exponential decay with a half life of 10 s.
#' To enable the buffer, \code{buffer_length_time} and \code{buffer_length_items}
#' should be made non-zero, and \code{only_learn_from_buffer} and
#' \code{only_predict_from_buffer} should be set to \code{TRUE}.
#' Likewise, retrieval noise is enabled by setting \code{noise} to a non-zero value,
#' and the short-term memory phase is enabled by setting \code{stm_duration}
#' to a non-zero value.
#' 
#' The names of the 'short-term memory' and 'long-term memory' phases
#' should be considered arbitrary in this context;
#' they do not necessarily correspond directly to their
#' psychological namesakes, but are instead simply terms of convenience.
#' 
#' The resulting PPM-Decay model uses interpolated smoothing with escape method A, 
#' and explicitly disables exclusion and update exclusion.
#' See \insertCite{Harrison2019;textual}{ppm} for details. 
#' 
#' @param alphabet_size
#' (Integerish scalar)
#' The size of the alphabet from which sequences are drawn.
#' 
#' @param order_bound
#' (Integerish scalar)
#' The model's Markov order bound.
#' 
#' @param ltm_weight
#' (Numeric scalar)
#' \eqn{w_2}, initial weight in the long-term memory phase.
#' 
#' @param ltm_half_life
#' (Numeric scalar)
#' \eqn{t_2}, half life of the long-term memory phase.
#' Must be greater than zero.
#' 
#' @param ltm_asymptote
#' (Numeric scalar)
#' \eqn{w_\infty}, asymptotic weight as time tends to infinity.
#' 
#' @param noise
#' (Numeric scalar)
#' \eqn{\sigma_\epsilon}, scale parameter for the retrieval noise distribution.
#' 
#' @param stm_weight
#' (Numeric scalar)
#' \eqn{w_1}, initial weight in the short-term memory phase.
#' 
#' @param stm_duration
#' (Numeric scalar)
#' \eqn{t_1}, temporal duration of the short-term memory phase, in seconds.
#' 
#' @param buffer_weight
#' (Numeric scalar)
#' \eqn{w_0}, weight during the buffer phase.
#' 
#' @param buffer_length_time
#' (Numeric scalar)
#' \eqn{n_b}, the model's temporal buffer capacity.
#' 
#' @param buffer_length_items
#' (Integerish scalar)
#' \eqn{t_b}, the model's itemwise buffer capacity.
#' 
#' @param only_learn_from_buffer
#' (Logical scalar)
#' If TRUE, then n-grams are only learned if they fit within
#' the memory buffer. The default value is \code{FALSE}.
#' 
#' @param only_predict_from_buffer
#' (Logical scalar)
#' If TRUE, then the context used for prediction is limited by the memory buffer.
#' Specifically, for a context to be used for prediction,
#' the first symbol within that context must still be within the buffer
#' at the point immediately before the predicted event occurs.
#' The default value is \code{FALSE}.
#' 
#' @param seed
#' Random seed for prediction generation.
#' By default this is linked with R's random seed, such that
#' reproducible behaviour can be ensured as usual with the 
#' \code{\link{set.seed}} function.
#' 
#' @return 
#' A PPM-decay model object. 
#' These objects have reference semantics.
#' 
#' @seealso 
#' \code{\link{new_ppm_simple}},
#' \code{\link{model_seq}}.
#' 
#' @md
#' 
#' @references
#'   \insertAllCited{}
#' 
#' @export
new_ppm_decay <- function(
  alphabet_size,
  order_bound = 10L,
  ltm_weight = 1,
  ltm_half_life = 10,
  ltm_asymptote = 0,
  noise = 0,
  stm_weight = 1,
  stm_duration = 0,
  buffer_weight = 1,
  buffer_length_time = 0,
  buffer_length_items = 0L,
  only_learn_from_buffer = FALSE,
  only_predict_from_buffer = FALSE,
  seed = sample.int(.Machine$integer.max, 1)
) {
  checkmate::qassert(alphabet_size, "X1")
  checkmate::qassert(order_bound, "X[0,)")
  checkmate::qassert(ltm_weight, "N1[0,)")
  checkmate::qassert(ltm_half_life, "N1(0,)")
  checkmate::qassert(ltm_asymptote, "N1[0,)")
  checkmate::qassert(noise, "N1[0,)")
  checkmate::qassert(stm_weight, "N1[0,)")
  checkmate::qassert(stm_duration, "N1[0,)")
  checkmate::qassert(buffer_length_time, "N1[0,)")
  checkmate::qassert(buffer_length_items, "X1[0,)")
  checkmate::qassert(buffer_weight, "N1[0,)")
  checkmate::qassert(only_learn_from_buffer, "B1")
  checkmate::qassert(only_predict_from_buffer, "B1")
  
  decay_par = list(
    ltm_weight = as.numeric(ltm_weight),
    ltm_half_life = as.numeric(ltm_half_life),
    ltm_asymptote = as.numeric(ltm_asymptote),
    noise = as.numeric(noise),
    stm_weight = as.numeric(stm_weight),
    stm_duration = as.numeric(stm_duration),
    buffer_weight = as.numeric(buffer_weight),
    buffer_length_time = as.numeric(buffer_length_time),
    buffer_length_items = as.integer(buffer_length_items),
    only_learn_from_buffer = as.logical(only_learn_from_buffer),
    only_predict_from_buffer = as.logical(only_predict_from_buffer)
  )
  
  new(
    ppm_decay, 
    alphabet_size = as.integer(alphabet_size),
    order_bound = as.integer(order_bound),
    decay_par = decay_par,
    seed = as.integer(seed)
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
