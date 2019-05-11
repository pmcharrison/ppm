#' Get n-gram weights
#' 
#' Tabulates weights for all possible n-grams of a given order.
#' 
#' @param mod 
#' A PPM model object as produced by (for example)
#' \code{\link{new_ppm_simple}} or \code{\link{new_ppm_decay}},
#' and subsequently trained on input sequences using \code{\link{model_seq}}.
#' 
#' @param order
#' (Integerish scalar)
#' The order (i.e. number of symbols) of the n-grams to retrieve.
#' 
#' @param pos
#' (Integerish scalar)
#' The nominal 'position' at which the n-gram counts are retrieved
#' (only relevant for decay-based models).
#' 
#' @param time
#' (Numeric scalar)
#' The nominal 'time' at which the n-grams are retrieved
#' (only relevant for decay-based models).
#' 
#' @param zero_indexed
#' (Logical scalar)
#' Whether the n-grams should be presented as zero-indexed (as opposed to one-indexed)
#' integer vectors.
#' 
#' @return 
#' Returns a \code{\link[tibble]{tibble}} where each row corresponds to an n-gram.
#' These n-grams are exhaustively enumerated from all possible symbols drawn from the model's alphabet.
#' The tibble contains n columns \code{elt_1}, \code{elt_1}, ... \code{elt_n},
#' corresponding to the n symbols in the n-gram,
#' and a column \code{weight}, identifying the weight of the specified n-gram.
#' 
#' @export
get_n_gram_weights  <- function(mod, order, pos = 1L, time = 0, zero_indexed = FALSE) {
  stopifnot(ppm::is_ppm_simple(mod))
  checkmate::qassert(order, "X1[1,)")
  
  alphabet <- seq(from = if (zero_indexed) 0L else 1L,
                  length.out = mod$alphabet_size)
  
  res <- seq_len(order) %>% 
    purrr::set_names(. ,paste("elt_", ., sep = "")) %>% 
    purrr::map(~ alphabet) %>% 
    expand.grid() %>% 
    tibble::as_tibble()
  
  res$weight <- purrr::pmap_dbl(res, function(...) 
    ppm::get_weight(mod, 
                    n_gram = as.integer(c(...)), 
                    zero_indexed = zero_indexed))
  
  res
}
