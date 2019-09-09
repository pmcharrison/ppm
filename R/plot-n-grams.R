#' Plot n-grams
#' 
#' This function takes a trained PPM model and plots transition probabilities 
#' computed by tabulating n-grams of length 1 and 2.
#' 
#' @details 
#' The output comprises two panels.
#' The top panel plots the empirical probability distribution of 1-grams;
#' this captures the relative frequencies of different symbols in the alphabet.
#' The bottom panel plots conditional probability distributions computed from 2-grams.
#' Each row corresponds to a maximum-likelihood probability distribution for the next symbol
#' conditioned on the preceding symbol indexed by that row.
#' Each column corresponds to a different continuation.
#' These 2-gram conditional probabilities are not plotted directly,
#' but are instead plotted relative to the corresponding 1-gram probabilities
#' (i.e. the 2-gram probability minus the 1-gram probability).
#' This helps the reader to separate 2-gram structure from 1-gram structure.
#' 
#' @param mod 
#' A PPM model object as produced by (for example)
#' \code{\link{new_ppm_simple}} or \code{\link{new_ppm_decay}},
#' and subsequently trained on input sequences using \code{\link{model_seq}}.
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
#' @param max_alphabet_size
#' If the model's alphabet size is larger than this value,
#' then the function will throw an error,
#' to protect the user from trying to plot prohibitively large
#' transition matrices.
#' 
#' @param zero_indexed
#' (Logical scalar)
#' If \code{zero_indexed = FALSE} (default), 
#' then the alphabet is mapped to ascending integers beginning at 1;
#' otherwise, the alphabet is mapped to ascending integers beginning at 0
#' (i.e. all symbols are decremented by 1).
#' 
#' @param heights
#' A numeric vector of length 2 specifying the relative heights of the 
#' top and bottom plot panel respectively.
#' 
#' @param bigram_fill_scale 
#' A \code{ggplot2} scale for the fill aesthetic of the bigram plot.
#' 
#' @export
#' 
#' @note 
#' This function requires the following additional packages: dplyr, ggplot2, and egg,
#' each of which can be installed using \code{install.packages} from CRAN.
plot_n_grams <- function(
  mod, pos = 1L, time = 0, max_alphabet_size = 30L,
  zero_indexed = FALSE, heights = c(0.25, 0.75),
  bigram_fill_scale = ggplot2::scale_fill_viridis_c("Probability (relative)")
) {
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("this function requires the dplyr package")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("this function requires the ggplot2 package")
  if (!requireNamespace("egg", quietly = TRUE))
    stop("this function requires the egg package")
  
  stopifnot(ppm::is_ppm(mod))
  if (mod$alphabet_size > max_alphabet_size)
    stop("cannot plot n-grams when the model's alphabet size (= ", mod$alphabet_size,
         ") exceeds 'max_alphabet_size' (= ", max_alphabet_size, ")")
  
  unigram_probs <- get_unigram_probabilities(mod, zero_indexed = zero_indexed)
  bigram_probs <- get_bigram_probabilities(mod, unigram_probs, zero_indexed = zero_indexed)
  
  egg::ggarrange(plot_unigrams(unigram_probs),
                 plot_bigrams(bigram_probs, bigram_fill_scale),
                 ncol = 1,
                 heights = heights)
}

get_unigram_probabilities <- function(mod, zero_indexed) {
  get_n_gram_weights(mod, order = 1L, zero_indexed = zero_indexed) %>% 
    dplyr::mutate(probability = .data$weight / sum(.data$weight))
}

get_bigram_probabilities <- function(mod, unigram, zero_indexed) {
  get_n_gram_weights(mod, order = 2L, zero_indexed = zero_indexed) %>% 
    dplyr::group_by(.data$elt_1) %>% 
    dplyr::mutate(probability = .data$weight / sum(.data$weight)) %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(tibble::tibble(elt_2 = unigram$elt_1,
                                    unigram_probability = unigram$probability),
                     by = "elt_2") %>% 
    dplyr::mutate(relative_probability = .data$probability - .data$unigram_probability)
}

plot_bigrams <- function(x, fill_scale) {
  alphabet <- sort(unique(x$elt_1))
  ggplot2::ggplot(x, ggplot2::aes_string(
    x = "elt_1", y = "elt_2", fill = "relative_probability")) +
    ggplot2::geom_tile(colour = "black", size = 0.5) +
    ggplot2::scale_x_continuous(breaks = alphabet,
                                minor_breaks = NULL,
                                name = "Continuation") +
    ggplot2::scale_y_continuous(breaks = alphabet,
                                minor_breaks = NULL,
                                name = "Context") +
    fill_scale +
    ggplot2::theme(legend.position = "bottom",
                   legend.justification = "centre") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(title.position = "top", 
                                                    # hjust = 0.5, # centres the title horizontally
                                                    title.hjust = 0,
                                                    label.position = "bottom",
                                                    ticks.colour = "black",
                                                    ticks.linewidth = 1,
                                                    frame.colour = "black",
                                                    frame.linewidth = 1)) 
}

plot_unigrams <- function(x) {
  alphabet <- sort(unique(x$elt_1))
  ggplot2::ggplot(x, ggplot2::aes_string(x = "elt_1", y = "probability")) +
    ggplot2::geom_bar(stat = "identity", colour = "black", fill = "#289b87") +
    ggplot2::scale_x_continuous(breaks = alphabet, minor_breaks = NULL, name = NULL) +
    ggplot2::scale_y_continuous("Probability")
}
