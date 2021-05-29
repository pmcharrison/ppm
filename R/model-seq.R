#' Model sequence
#' 
#' Analyses or generates a sequence using a PPM model.
#' 
#' @param model
#' A PPM model object as produced by (for example)
#' \code{\link{new_ppm_simple}} or \code{\link{new_ppm_decay}}.
#' 
#' @param seq
#' When \code{generate = FALSE} an integer vector defining the input sequence
#' (equivalently a numeric vector containing solely integers,
#' or a factor vector, both of which which will be coerced to integer vectors).
#' When \code{generate = TRUE}, an integer scalar giving the desired length 
#' of the generated sequence.
#' 
#' @param time
#' (NULL or a numeric vector)
#' Timepoints corresponding to each element of the sequence.
#' Only used by certain model types (e.g. decay-based models).
#' 
#' @param zero_indexed
#' (Logical scalar)
#' Whether or not the \code{seq} argument is 0-indexed 
#' (i.e. drawn from an alphabet with a minimum value of 0).
#' If FALSE, it is assumed that the sequence is 1-indexed
#' (i.e. drawn from an alphabet with a minimum value of 1).
#' 
#' @param train
#' (Logical scalar)
#' Whether or not the model should learn from the incoming sequence.
#' 
#' @param predict
#' (Logical scalar)
#' Whether or not to generate predictions for each element of
#' the incoming sequence.
#' 
#' @param return_distribution
#' (Logical scalar)
#' Whether or not to return the conditional distribution over each
#' potential continuation as part of the model output
#' (ignored if \code{predict = FALSE}).
#' 
#' @param return_entropy
#' (Logical scalar)
#' Whether or not to return the entropy of each event prediction
#' (ignored if \code{predict = FALSE}).
#' 
#' @param generate
#' (Logical scalar)
#' If \code{TRUE}, the output will correspond to a newly generated sequence
#' with length as specified by the \code{seq} argument, 
#' produced by sampling from the model's predictive distribution.
#' The default is \code{FALSE}.
#' 
#' @return 
#' A \code{\link[tibble]{tibble}} which will be empty if \code{predict = FALSE}
#' and otherwise will contain one row for each element in the sequence,
#' with the following columns:
#' - \code{symbol} - the symbol being predicted. This should be identical
#' to the input argument \code{seq}, with indexing determined by 
#' the argument \code{0-indexed}.
#' - \code{model_order} - the model order used for generating predictions.
#' - \code{information_content} - the information content
#' (i.e. negative log probability, base 2) of the observed symbol.
#' - \code{entropy} - the expected information content when 
#' predicting the symbol.
#' - \code{distribution} - the predictive probability distribution for the 
#' symbol, conditioned on the preceding symbols.
#' 
#' @md 
#' @rdname model_seq
#' @export
model_seq <- function(model,
                      seq,
                      time = NULL,
                      zero_indexed = FALSE,
                      train = TRUE,
                      predict = TRUE,
                      return_distribution = TRUE,
                      return_entropy = TRUE,
                      generate = FALSE) {
  if (is.character(seq)) {
    stop("'seq' cannot be a character vector; please provide a factor representation instead.")
  }
  if (generate && length(seq) == 1 && is.numeric(seq)) {
    seq <- integer(seq)
  }
  
  seq <- as.integer(seq)
  
  stopifnot(is_ppm(model))
  checkmate::qassert(seq, "X")
  checkmate::qassert(zero_indexed, "B1")
  checkmate::qassert(train, "B1")
  checkmate::qassert(predict, "B1")
  checkmate::qassert(return_distribution, "B1")
  checkmate::qassert(return_entropy, "B1")
  checkmate::qassert(generate, "B1")
  stopifnot(is.null(time) || is.numeric(time))
  
  if (!generate) {
    if (zero_indexed) {
      if (any(seq < 0L))
        stop("all elements of 'seq' must be greater than or equal to 0")
      if (any(seq >= model$alphabet_size))
        stop("all elements of 'seq' must be less than the model's alphabet size", 
             " (", model$alphabet_size, ")")
    } else {
      if (any(seq < 1L))
        stop("all elements of 'seq' must be greater than or equal to 1")
      if (any(seq > model$alphabet_size))
        stop("all elements of 'seq' must be less than or equal to the model's alphabet size",
             " (", model$alphabet_size, ")")
    }
  }
  
  if (is.factor(seq) && length(model$alphabet_levels > 0) && !identical(levels(seq), model$alphabet_levels)) {
    warning("sequence's factor levels seemed inconsistent with model$alphabet_levels")
  }
  
  if (any(diff(time) < 0)) stop("decreasing values of time are not permitted")
  
  if (is_ppm_decay(model) && (length(seq) != length(time)))
    stop("for 'ppm_decay' models, ", 
         "'time' must be provided as a numeric vector ",
         "with the same length as 'seq'")
  
  if (!zero_indexed) seq <- seq - 1L
  
  res <- model$model_seq(
    x = seq,
    time = as.numeric(time),
    train = train,
    predict = predict,
    return_distribution = return_distribution,
    return_entropy = return_entropy,
    generate = generate
  )
  df <- res$as_tibble()
  
  if (!zero_indexed) df$symbol <- df$symbol + 1L
  
  if (length(model$alphabet_levels) > 0) {
    df$symbol <- factor(model$alphabet_levels[df$symbol], levels = model$alphabet_levels)
  }
  
  df
}
