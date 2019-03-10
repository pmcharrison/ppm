library(ppm)
library(tidyverse)

x <- new(ppm_simple, 
         alphabet_size = 100,
         order_bound = 4, 
         shortest_deterministic = TRUE,
         exclusion = TRUE,
         update_exclusion = TRUE,
         escape = "C")

x <- new(ppm_decay,
         alphabet_size = 100,
         order_bound = 10,
         decay_par = list(
           buffer_length_items = 10,
           buffer_weight = 0.75,
           stm_half_life = 1,
           stm_weight = 0.25,
           ltm_weight = 0,
           noise = 0.5
         ))

y <- x$model_seq(c(1, 2, 3, 1, 2, 1, 1, 4, 5), 
                 train = TRUE, 
                 predict = TRUE, 
                 keep_distribution = TRUE, 
                 keep_entropy = TRUE)

x$as_tibble() %>% 
  dplyr::mutate(n_gram = map_chr(n_gram, paste, collapse = " "))

y$as_tibble()

y <- x$model_seq(0:5, 
                 train = FALSE, 
                 predict = TRUE, 
                 keep_distribution = FALSE, 
                 keep_entropy = FALSE)
y$as_tibble()

class(y$as_list()$symbol)
y$as_tibble()

y$as_tibble()

tibble::as_tibble(y$as_data_frame())

new(ppm:::symbol_prediction, rnorm(5))$distribution
