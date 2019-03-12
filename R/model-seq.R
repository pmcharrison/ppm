#' Model sequence
#' 
#' Models a sequence using a PPM model.
#' @return a \code{\link[tibble]{tibble}}.
#' @rdname model_seq
#' @export
model_seq <- function(model,
                      seq,
                      time = NULL,
                      train = TRUE,
                      predict = TRUE,
                      return_distribution = TRUE,
                      return_entropy = TRUE) {
  stopifnot(is_ppm(model))
  checkmate::qassert(seq, "X")
  checkmate::qassert(train, "B1")
  checkmate::qassert(predict, "B1")
  checkmate::qassert(return_distribution, "B1")
  checkmate::qassert(return_entropy, "B1")
  stopifnot(is.null(time) || is.numeric(time))
  
  if (is_ppm_decay(model) && (length(seq) != length(time)))
    stop("for 'ppm_decay' models, ", 
         "'time' must be provided as a numeric vector ",
         "with the same length as 'seq'")
  
  res <- model$model_seq(
    x = as.integer(seq),
    time = as.numeric(time),
    train = train,
    predict = predict,
    return_distribution = return_distribution,
    return_entropy = return_entropy
  )
  res$as_tibble()
}
